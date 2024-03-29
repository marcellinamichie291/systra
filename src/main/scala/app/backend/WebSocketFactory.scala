package app.backend

import com.github.imomushi8.systra.core.entity.Chart
import com.github.imomushi8.systra.chart.WebSocketStream

import app.Envs._
import app.bitflyer.BitFlyerWS

import cats.effect._
import fs2._
import scala.concurrent.duration.FiniteDuration

object WebSocketFactory:

  def get(marketName: String): IO[WebSocketStream] = marketName match
    case "bitflyer_btc" => bitflyer_btc
    case _              => ??? // FIXME

  def bitflyer_btc = for
    apiKey    <- BITFLYER_API_KEY.load[IO]
    apiSecret <- BITFLYER_API_SECRET.load[IO]
    channel   <- BITFLYER_PUBLIC_CHANNEL.load[IO]
  yield BitFlyerWS(apiKey.value, apiSecret.value, channel)