package com.github.imomushi8.systra.core.data

import cats.effect.IO
import fs2.Stream

import sttp.model.Uri
import sttp.client3._
import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend
import sttp.capabilities.fs2.Fs2Streams
import sttp.ws.{WebSocket, WebSocketFrame}

trait WebSocketStream:
  val uri: Uri
  
  /** WebSocketの処理を実装 */
  def callback(websocket: Stream[IO, WebSocketFrame.Data[?]]): Stream[IO, WebSocketFrame]

  /** 開始メソッド */
  def begin(): Ops = new Ops
 
  class Ops:
    /** Demoの終了メソッド */
    def end() = AsyncHttpClientFs2Backend
      .resource[IO]()
      .use { backend =>
        basicRequest
          .response(asWebSocketStreamAlways(Fs2Streams[IO])(callback))
          .get(uri)
          .send(backend)
      }
      .>> { IO.println("Done DemoTrade") }