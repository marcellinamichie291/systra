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

trait WebSocketStream extends LazyLogging:
  /**
   * WebSocketの接続URIをciris.ConfigValueで渡す
   */
  val configUri: ConfigValue[Effect, Uri]

  /**
   * WebSocketの処理を実装
   */
  def callback(websocket: Stream[IO, WebSocketFrame.Data[?]]): Stream[IO, WebSocketFrame]

  /**
   * WebSocket実行
   */
  def apply(haltOnSignal: Signal[IO, Boolean]): IO[Response[Unit]] = for
    uri <- configUri.load[IO]
    res <- run(uri, haltOnSignal)
  yield res

  def run(uri: Uri,
          haltOnSignal: Signal[IO, Boolean]): IO[Response[Unit]] =
    AsyncHttpClientFs2Backend.resource[IO]().use { backend =>
      basicRequest
        .response(asWebSocketStreamAlways(Fs2Streams[IO]) {
          _.through(callback)
            .interruptWhen(haltOnSignal) // haltOnSignalの中身がtrueになったら中断する仕組み
            .onComplete {
              logger.info(
                "WebSocket into onComplete phase. So WebSocket is close.")
              Stream.emit(WebSocketFrame.close)
            }
        })
        .get(uri)
        .send(backend)
    }
