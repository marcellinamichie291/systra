package app

import ciris._
import sttp.client3._
import sttp.model.Uri

object WebSocketUrl:
  val BITFLYER_WS_URL: ConfigValue[Effect, Uri] = default(uri"wss://ws.lightstream.bitflyer.com/json-rpc")
