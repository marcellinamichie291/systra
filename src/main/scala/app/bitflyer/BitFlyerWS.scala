package app.bitflyer

import app.WebSocketUrl
import app.bitflyer.BFReq._
import app.bitflyer.BFRes._
import app.bitflyer.BFUtils.{given_Initial_ConnectionState ,ConnectionState}

import com.github.imomushi8.systra.chart._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.util._

import cats.implicits._
import cats.effect._
import fs2.{Stream, Pipe}

import io.circe.{Encoder, Decoder, DecodingFailure}
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._

import ciris._

import sttp.client3._
import sttp.model.Uri
import sttp.ws.{WebSocket, WebSocketFrame}

import com.typesafe.scalalogging.LazyLogging
import java.time.temporal.TemporalAmount
import scala.concurrent.duration._

class BitFlyerWS(apiKey:    String,
                 apiSecret: String,
                 channel:   Channel) extends WebSocketStream, LazyLogging:

  override val configUri: ConfigValue[Effect, Uri] = WebSocketUrl.BITFLYER_WS_URL
  override val configMaxConnectCount: ConfigValue[Effect, Int] = default(5) // 変更するならここで変える

  override def toChart(period: FiniteDuration)(websocket: Stream[IO, WebSocketFrame.Data[?]]): Stream[IO, (Option[WebSocketFrame], Option[Chart])] = 
    lazy val stream = websocket
      .through(dataToResponse) // circeによるJSONのparse
      .through(responseToState) // parseしたものをさらに整形
      .flatMap {
        case ConnectionState( true, false, tickers) => Stream.emit((Some(BFUtils.subscribeText(channel)), tickers))
        case ConnectionState( true,  true, tickers) => Stream.emit((        Option.empty[WebSocketFrame], tickers))
        case ConnectionState(false,     _, tickers) => Stream.empty
      }
      .flatMap { case (frameOp, tickers) => 
        Stream.emit((frameOp, Option.empty[ExecutionInfo])) ++ Stream.emits(tickers.sortBy(_.exec_date).map { ticker => (None, Some(ticker)) })
      }
      .broadcastThrough(
        tickerToChart(period),
        collectWSFrame
      )
      
    Stream
      .emit((Some(BFUtils.authText(apiKey, apiSecret)), None))
      .append(stream)

  /* tickerをchartに変換する */
  private def tickerToChart(period: FiniteDuration)(stream: Stream[IO, (Option[WebSocketFrame], Option[ExecutionInfo])]): Stream[IO, (Option[WebSocketFrame], Option[Chart])] = 
    stream
      .collect { case (_, Some(ticker)) => ticker }
      .through(BFUtils.toChart(period))
      .map { chart => (None, Some(chart)) }

  /** WebSocketFrameだけ回収する */
  private def collectWSFrame(stream: Stream[IO, (Option[WebSocketFrame], Option[ExecutionInfo])]): Stream[IO, (Option[WebSocketFrame], Option[Chart])] =
    stream.collect { case (frameOp @ Some(_), _) => (frameOp, None) }

      
  private val dataToResponse: Pipe[IO, WebSocketFrame.Data[?], Either[io.circe.Error, JsonRpc]] = _
    .map {
      case WebSocketFrame.Text(payload, _, _) =>
        logger.debug(s"GET for Websocket: $payload")
        parse(payload).flatMap { json => json
          .as[JsonRpcRes[Boolean]]
          .leftFlatMap{ _=>json.as[JsonRpcReq[BFRes.Execution]] }
        }
      case frame => throw new RuntimeException(s"WebSocketFrame is invalid. frame: $frame")
    }

  private val responseToState: Pipe[IO, Either[io.circe.Error, JsonRpc], ConnectionState] = _
    .scan(Initial[ConnectionState]()) { case (state, eitherRes) => eitherRes match
      /* 認証結果の場合 */
      case Right(JsonRpcRes(_, 1, Some(true), None)) =>
        logger.info("BitFlyer Auth is Succeeded")
        state.copy(passAuth=true)

      /* チャンネル購読結果の場合 */
      case Right(JsonRpcRes(_, 2, Some(true), None)) =>
        logger.info(s"Channel '$channel' subscribe is Succeeded")
        state.copy(isSubscribed=true)

      /* チャンネル購読後のデータ */
      case Right(JsonRpcReq(_, _, Execution(_, message), _)) => state.copy(tickers=message)

      /* RequestError */
      case Right(JsonRpcRes(_, _, None, Some(error))) => throw new RuntimeException(s"Request Error: ${error.asJson.noSpaces}")
      /* どれにも当てはまらない場合 */
      case Right(res) => throw new RuntimeException(s"Pattern match Exception: response: $res")
      /* ParseError */
      case Left(failure) => throw failure
    }