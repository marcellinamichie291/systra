package app.demo

import app.demo.Demo
import app.bitflyer._
import app.bitflyer.BFReq._

import com.github.imomushi8.systra.chart._
import com.github.imomushi8.systra.virtual.VirtualMarket
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.market.{Tradable, OK, TradeState}

import cats.effect.IO
import fs2.{Stream, Pipe}

import io.circe.{Encoder, Decoder, DecodingFailure}
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._

import sttp.ws.WebSocketFrame
import com.github.imomushi8.systra.core.util.Initial
import java.time.temporal.TemporalAmount

object DemoBitFlyerWS:
  def apply[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])],
                             firstCapital: Price,
                             apiKey:       String,
                             apiSecret:    String,
                             channel:      Channel,
                             period:       TemporalAmount) = new BitFlyerWS(apiKey, apiSecret, channel, period) {
    override def executeTrade(chartStream: Stream[IO, Chart]): Stream[IO, WebSocketFrame] = chartStream
      .head
      .flatMap { head => chartStream
        .through(Demo(brains, firstCapital, head))
        .drain
      }
  }