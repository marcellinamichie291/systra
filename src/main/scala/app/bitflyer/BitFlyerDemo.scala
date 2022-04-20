package app.bitflyer

import app.Envs

import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.demo.Demo
import com.github.imomushi8.systra.virtual.VirtualMarket
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.data._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.market.{Tradable, OK, TradeState}

import cats.effect.IO
import fs2.{Stream, Pipe}

import io.circe.{Encoder, Decoder, DecodingFailure}
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._

import sttp.client3._
import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend
import sttp.capabilities.fs2.Fs2Streams
import sttp.ws.{WebSocket, WebSocketFrame}

class BitFlyerDemo[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])],
                                    firstCapital: Price,
                                    apiKey:       String,
                                    apiSecret:    String,
                                    channel:      Channel) extends BFStream(apiKey, apiSecret, channel), Demo:
  override def executeTrade(chartStream: Stream[IO, Chart]): Stream[IO, WebSocketFrame] = chartStream
    .head
    .flatMap { head => chartStream
      //.broadcastThrough(summerize(brains, firstCapital, head)*)
      //.map(_.toString)
      .printlns
      .drain
    }
