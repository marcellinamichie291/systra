package app.demo

import app.demo.BitFlyerRes.ExecutionInfo

import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.util._

import io.circe.syntax._
import sttp.ws.{WebSocket, WebSocketFrame}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex
import java.time.LocalDateTime
import java.time.ZonedDateTime
import java.time.ZoneId
import java.time.Instant

object BFUtils {
  case class ConnectionState(passAuth: Boolean, 
                           isSubscribed: Boolean,
                           tickers: Seq[ExecutionInfo])

  given Initial[ConnectionState] = new Initial { def empty() = ConnectionState(false,false,Nil) }

  private def getTimestamp() = System.currentTimeMillis
  private def getNonce() = scala.util.Random.alphanumeric.take(16).mkString
  private def hmacSha256(secret: String, data: String) = 
    val algo = "HMacSHA256"
    val mac:Mac = Mac.getInstance(algo)
    mac.init(new SecretKeySpec(secret.getBytes(), algo))
    val signBytes = mac.doFinal(data.getBytes())
    new String(Hex.encodeHex(signBytes))

  def authText(apiKey: String, apiSecret: String): WebSocketFrame.Text =
    val timestamp = getTimestamp()
    val nonce     = getNonce()
    WebSocketFrame.text(BitFlyerReq(
      method="auth",
      params=AuthParams(apiKey, timestamp, nonce, hmacSha256(apiSecret, s"${timestamp}${nonce}")).asJson,
      id=1).asJson.toString)

  def subscribeText(publicChannel: Channel): WebSocketFrame.Text = WebSocketFrame.text(BitFlyerReq(
    method="subscribe",
    params=SubscribeParams(publicChannel).asJson,
    id=2).asJson.toString)

  def parseLocalDatetime(dateStr: String): TimeStamp = Instant.parse(dateStr).atZone(ZoneId.systemDefault()).toLocalDateTime
}
