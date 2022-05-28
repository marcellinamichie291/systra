package com.github.imomushi8.systra.chart

import com.github.imomushi8.systra.virtual.VirtualMarket
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.market.{Tradable, OK, TradeState}

import cats.effect.IO
import fs2.{Stream, Pipe}

import ciris._

import sttp.model.Uri
import sttp.client3._
import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend
import sttp.capabilities.fs2.Fs2Streams
import sttp.ws.{WebSocket, WebSocketFrame}
import fs2.concurrent.SignallingRef
import com.typesafe.scalalogging.LazyLogging

import app.model.AppStatus
import app.model.service.Service
import fs2.concurrent.Signal

import concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import cats.data.Kleisli

trait WebSocketStream extends LazyLogging:
  /** WebSocketの接続URIをciris.ConfigValueで渡す */
  val configUri: ConfigValue[Effect, Uri]

  /** WebSocketの接続上限回数。デフォルトは5回とする */
  val configMaxConnectCount: ConfigValue[Effect, Int] = default(5)

  /** WebSocketの再接続待機時間。デフォルトは30秒とする */
  val configReconnectDelay: ConfigValue[Effect, FiniteDuration] = default(30.second)

  /** WebSocketFrameをChartに変換する処理を実装 */
  def toChart(websocket: Stream[IO, WebSocketFrame.Data[?]]): Stream[IO, (WebSocketFrame, Option[Chart])]

  /** WebSocket実行 */
  def apply(trade: Pipe[IO, Chart, Unit]): Kleisli[IO, Signal[IO, Boolean], Unit] = Kleisli { haltOnSignal =>
    for
      uri             <- configUri.load[IO]
      maxConnectCount <- configMaxConnectCount.load[IO]
      delay           <- configReconnectDelay.load[IO]
      ?               <- run(trade, haltOnSignal, uri, maxConnectCount, delay)
    yield ()
  }

  private def run(trade:           Pipe[IO, Chart, Unit],
                  haltOnSignal:    Signal[IO, Boolean],
                  uri:             Uri,
                  maxConnectCount: Int,
                  delay:           FiniteDuration): IO[Response[Unit]] = AsyncHttpClientFs2Backend
    .resource[IO]().use { backend =>
      basicRequest
        .response(asWebSocketStreamAlways(Fs2Streams[IO]) { websocket =>
          websocket
            .through(toChart)                              // Chartに変換する
            .through(retryConnect(maxConnectCount, delay)) // 再接続が必要な場合は再接続する
            .broadcastThrough(
              reply,         // WebSocketFrameの返答
              execute(trade) // トレード実行
            )
            .interruptWhen(haltOnSignal)
            .onComplete {
              logger.info("WebSocket is on complete.")
              // グレースフルに（全部決済させてから）停止したいから、ここにshutdown関数を入れてもらう？
              Stream.emit(WebSocketFrame.close)
            }
        })
        .get(uri)
        .send(backend)
    }

  /** 規定回数だけ再接続を行う */
  private def retryConnect[I](maxConnectCount: Int, 
                              delay: FiniteDuration): Pipe[IO, I, I] = _
    .attempts(Stream.constant[IO, FiniteDuration](delay)) // 失敗したらdelay秒後に繰り返し
    .zipWithScan1(1) {
      case (connectCnt, Left(_))  => connectCnt + 1
      case (connectCnt, Right(_)) => connectCnt
    }
    .takeWhile { case (_, connectCount) => connectCount < maxConnectCount } // 接続規定回数までデータを取得する
    .flatMap {
      case (Left(t), idx) =>
        logger.error(s"Exception occured. ${t.getMessage}", t)
        logger.info(s"WebSocket reconnect count: $idx/$maxConnectCount")
        Stream.empty // エラー時は何も出力しない
      case (Right(s), _)  =>
        Stream.emit(s) // エラーでなければEitherをはがすだけ
    }

  /** WebSocketFrameを返す */
  private def reply: Pipe[IO, (WebSocketFrame, Option[Chart]), WebSocketFrame] = _.map(_._1)

  /** トレード実行 */
  private def execute(trade: Pipe[IO, Chart, Unit]): Pipe[IO, (WebSocketFrame, Option[Chart]), WebSocketFrame] =
    _.collect { case (_, Some(chart)) => chart }.through(trade).drain
