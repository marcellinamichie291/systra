import io.circe.{Encoder}
import io.circe.generic.semiauto._
import io.circe.syntax._

import org.scalatest.flatspec.AnyFlatSpec

class CirceSpec extends AnyFlatSpec:
  sealed trait Params
  case class AuthReq(api_key:   String,
                    timestamp: Int,
                    nonce:     String,
                    signature: String) extends Params

  case class Auth(jsonrpc: String="2.0", 
                  method: String, 
                  params: Params)

  it should "no error" in {
    given encoder: Encoder[Auth] = deriveEncoder
    given authEncoder: Encoder[AuthReq] = deriveEncoder
    val circed = Auth(
      method="auth",
      params=AuthReq(
        "api",
        0,
        "nonce",
        "sign"
      )
    ).asJson.toString

    println(circed)
  }