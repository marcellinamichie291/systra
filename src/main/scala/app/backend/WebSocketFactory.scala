package app.backend

import com.github.imomushi8.systra.core.entity.Chart
import com.github.imomushi8.systra.chart.WebSocketStream

import app.Envs._
import app.bitflyer.BitFlyerWS

import cats.effect._
import fs2._
import java.time.temporal.TemporalAmount

class WebSocketFactory(executePipe: Pipe[IO, Chart, Unit], period: TemporalAmount):
  def get(marketName: String): IO[WebSocketStream] = marketName match
    case "bitflyer_btc" => bitflyer_btc
    case _ => ???

  def bitflyer_btc = for
    apiKey    <- BITFLYER_API_KEY.load[IO]
    apiSecret <- BITFLYER_API_SECRET.load[IO]
    channel   <- BITFLYER_PUBLIC_CHANNEL.load[IO]
  yield
    BitFlyerWS(executePipe, apiKey.value, apiSecret.value, channel, period)
