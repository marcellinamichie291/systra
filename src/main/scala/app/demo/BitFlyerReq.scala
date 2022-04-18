package app.demo

import io.circe.Json
import io.circe.Encoder
import io.circe.generic.semiauto._

given encoder: Encoder[BitFlyerReq] = deriveEncoder
given authEncoder: Encoder[AuthParams] = deriveEncoder
given subscribeEncoder: Encoder[SubscribeParams] = deriveEncoder

type Channel = String

case class BitFlyerReq(jsonrpc: String="2.0", 
                       method:  String, 
                       params:  Json,
                       id:      Int)

sealed trait Params
case class AuthParams(api_key:   String,
                      timestamp: Long,
                      nonce:     String,
                      signature: String) extends Params
case class SubscribeParams(channel: Channel) extends Params

