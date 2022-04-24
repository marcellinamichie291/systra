package app.bitflyer

import io.circe.{Json,Encoder}
import io.circe.generic.semiauto._

object BFReq:
  given Encoder[Auth] = deriveEncoder
  given Encoder[Subscribe] = deriveEncoder

  type Channel = String

  case class Auth(api_key:   String,
                  timestamp: Long,
                  nonce:     String,
                  signature: String)

  case class Subscribe(channel: Channel)