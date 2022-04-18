package app.demo

import io.circe.{Encoder, Decoder, DecodingFailure}
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._

import sttp.model.Uri
import sttp.client3._
import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend
import sttp.capabilities.fs2.Fs2Streams
import sttp.ws.{WebSocket, WebSocketFrame}

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex

object BFUtils {
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
    params=publicChannel.asJson,
    id=2).asJson.toString)
}
