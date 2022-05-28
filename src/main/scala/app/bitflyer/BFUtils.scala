package app.bitflyer

import app.bitflyer.BFReq._
import app.bitflyer.BFRes._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.util._

import io.circe.syntax._

import cats._
import cats.implicits._
import cats.effect._
import fs2._
import sttp.ws.{WebSocket, WebSocketFrame}

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex

import java.time.{ZoneId, Instant}
import java.time.temporal.TemporalAmount

object BFUtils:
  case class ConnectionState(passAuth: Boolean, 
                             isSubscribed: Boolean,
                             tickers: Seq[BFRes.ExecutionInfo])

  given Initial[ConnectionState] = new Initial { def empty() = ConnectionState(false,false,Nil) }

  private def getTimestamp() = System.currentTimeMillis
  private def getNonce() = scala.util.Random.alphanumeric.take(16).mkString
  private def hmacSha256(secret: String, data: String) = 
    val algo = "HMacSHA256"
    val mac:Mac = Mac.getInstance(algo)
    mac.init(new SecretKeySpec(secret.getBytes(), algo))
    val signBytes = mac.doFinal(data.getBytes())
    new String(Hex.encodeHex(signBytes))

  def authText(apiKey: String, apiSecret: String): WebSocketFrame =
    val timestamp = getTimestamp()
    val nonce     = getNonce()
    WebSocketFrame.text(JsonRpcReq[Auth](
      method="auth",
      params=Auth(apiKey, timestamp, nonce, hmacSha256(apiSecret, s"${timestamp}${nonce}")),
      id=Some(1)).asJson.toString)

  def subscribeText(publicChannel: Channel): WebSocketFrame = WebSocketFrame.text(JsonRpcReq[Subscribe](
    method="subscribe",
    params=Subscribe(publicChannel),
    id=Some(2)).asJson.toString)

  def parseLocalDatetime(dateStr: String): TimeStamp = Instant.parse(dateStr).atZone(ZoneId.systemDefault()).toLocalDateTime

  /** TickerからChartに成形する */
  def createChart(period: TemporalAmount,
                  head:   Chart): Pipe[IO, ExecutionInfo, Chart] = _
    .scan(head.asLeft[(Chart, ExecutionInfo)]) { (either, ticker) => 
      val execDate = BFUtils.parseLocalDatetime(ticker.exec_date)
      either match
      // Leftであれば更新していく
        case Left(Chart(open, high, low, close, volume, datetime)) =>
          if (execDate compareTo datetime.plus(period)) < 0 then // 規定時間内の場合
            Chart(open, high max ticker.price, low min ticker.price, ticker.price, volume + ticker.size, datetime).asLeft[(Chart, ExecutionInfo)]
          else
            (Chart(open, high, low, close, volume, datetime), ticker).asRight[Chart] 

        // Rightであればリセットする
        case Right(_, ExecutionInfo(_,_,prevPrice,prevSize,prevExecDate,_,_)) =>
          val prevDatetime = BFUtils.parseLocalDatetime(prevExecDate)
          Chart(prevPrice, ticker.price max prevPrice, ticker.price min prevPrice, ticker.price, ticker.size+prevSize, prevDatetime).asLeft[(Chart, ExecutionInfo)]
    }
    .collect { case Right((chart, _)) => chart }