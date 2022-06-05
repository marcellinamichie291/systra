package app.bitflyer

import app.bitflyer.BFReq._
import app.bitflyer.BFRes._

import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.entity.UnixTimeStamp._
import com.github.imomushi8.systra.core.entity.chartMonoid
import com.github.imomushi8.systra.core.util._

import io.circe.syntax._

import cats._
import cats.implicits._
import cats.effect._

import fs2._
import fs2.timeseries.{TimeStamped, TimeSeries}

import sttp.ws.{WebSocket, WebSocketFrame}

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex

import java.time.{ZoneId, Instant}
import java.time.temporal.TemporalAmount
import scala.concurrent.duration._
import java.time.LocalDateTime

object BFUtils:
  case class ConnectionState(passAuth: Boolean, isSubscribed: Boolean, tickers: Seq[BFRes.ExecutionInfo])

  given Initial[ConnectionState] = new Initial { def empty() = ConnectionState(false, false, Nil) }

  private def getTimestamp()                           = System.currentTimeMillis
  private def getNonce()                               = scala.util.Random.alphanumeric.take(16).mkString
  private def hmacSha256(secret: String, data: String) =
    val algo      = "HMacSHA256"
    val mac: Mac  = Mac.getInstance(algo)
    mac.init(new SecretKeySpec(secret.getBytes(), algo))
    val signBytes = mac.doFinal(data.getBytes())
    new String(Hex.encodeHex(signBytes))

  def authText(apiKey: String, apiSecret: String): WebSocketFrame =
    val timestamp = getTimestamp()
    val nonce     = getNonce()
    WebSocketFrame.text(
      JsonRpcReq[Auth](
        method = "auth",
        params = Auth(apiKey, timestamp, nonce, hmacSha256(apiSecret, s"${timestamp}${nonce}")),
        id = Some(1)).asJson.toString)

  def subscribeText(publicChannel: Channel): WebSocketFrame = WebSocketFrame.text(
    JsonRpcReq[Subscribe](method = "subscribe", params = Subscribe(publicChannel), id = Some(2)).asJson.toString)

  def parseTimeStamp(dateStr: String): TimeStamp = UnixTimeStamp.parseSimple(dateStr)

  def convertChart: ExecutionInfo => Chart = {
    case ExecutionInfo(_, _, price, size, exec_date, _, _) =>
      Chart(price, price, price, price, size, parseTimeStamp(exec_date))
  }

  def toChart(period: FiniteDuration)(input: Stream[IO, ExecutionInfo]): Stream[IO, Chart] =
    val series = input.map(convertChart).through(aggregateTicker(period)).dropWhile(_.value.isEmpty)

    series.head.flatMap {
      case TimeStamped(firstTime, firstChartOp) =>
        val first = firstChartOp.get.setTime((firstTime - period).toMillis)

        series.scan(first) {
          case (Chart(_, _, _, close, _, _), TimeStamped(time, None)) =>
            Chart(close, close, close, close, 0, (time - period).toMillis.toTimeStamp) // 変化がない状態
          case (_, TimeStamped(time, Some(chart))) => 
            chart.setTime((time - period).toMillis)
        }
    }

  def aggregateTicker[T: Semigroup](period: FiniteDuration)(input: Stream[IO, T]): Stream[IO, TimeStamped[Option[T]]] =
    val aggregateInterval = TimeStamped.rate[Option[T], Seq[T]](period)(Vector.from).toPipe[IO]

    val ticker = Stream.fixedDelay[IO](period).evalMap(_ => TimeStamped.now[IO, Option[T]](None))

    input
      .evalMap(TimeStamped.now[IO, T])
      .map(tsa => tsa.map(Some(_): Option[T]))
      .merge(ticker)
      .through(aggregateInterval)
      .map {
        case ts @ TimeStamped(_, Nil) => ts.map(_ => None)
        case ts                       => ts.map(_.reduceLeft(_ |+| _).some)
      }
