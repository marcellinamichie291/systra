package com.github.imomushi8.systra.chart

import com.github.imomushi8.systra.virtual.VirtualMarket
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.data._
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

trait WebSocketStream:
  /** WebSocketの接続URIをciris.ConfigValueで渡す */
  val configUri: ConfigValue[Effect, Uri]
  
  /** WebSocketの処理を実装 */
  def callback(websocket: Stream[IO, WebSocketFrame.Data[?]]): Stream[IO, WebSocketFrame]
  
  /** WebSocket実行 */
  def apply():IO[Response[Unit]] = for
    uri <- configUri.load[IO]
    res <- run(uri)
  yield res
    
  def run(uri: Uri):IO[Response[Unit]] = AsyncHttpClientFs2Backend
    .resource[IO]()
    .use { backend => basicRequest
      .response(asWebSocketStreamAlways(Fs2Streams[IO])(callback))
      .get(uri)
      .send(backend)
    }
