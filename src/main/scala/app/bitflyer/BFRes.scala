package app.bitflyer

import io.circe.{Json, Encoder, Decoder}
import io.circe.generic.semiauto._

object BFRes:
  given Decoder[ExecutionInfo] = deriveDecoder
  given Decoder[Execution]     = deriveDecoder

  case class Execution(channel: String,
                       message: Seq[ExecutionInfo])

  case class ExecutionInfo(id:                             Long,
                           side:                           String,
                           price:                          Double,
                           size:                           Double,
                           exec_date:                      String,
                           buy_child_order_acceptance_id:  String,
                           sell_child_order_acceptance_id: String)