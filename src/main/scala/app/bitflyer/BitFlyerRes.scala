package app.bitflyer

import cats.implicits._

import io.circe.{Json, Encoder, Decoder}
import io.circe.generic.semiauto._
import io.circe.syntax._
import java.time.LocalDateTime
import com.github.imomushi8.systra.core.util.Initial

sealed trait BitFlyerRes

object BitFlyerRes:
  given errorDecoder:     Decoder[Message]       = deriveDecoder
  given paramsDecoder:    Decoder[Params]        = deriveDecoder
  given executionDecoder: Decoder[ExecutionInfo] = deriveDecoder

  given resResultDecoder:     Decoder[Result]     = deriveDecoder
  given resErrorDecoder:      Decoder[Error]      = deriveDecoder
  given resExecutionsDecoder: Decoder[Executions] = deriveDecoder

  given messageEncoder: Encoder[Message] = deriveEncoder

  case class Error(jsonrpc: String,
                   id:      Int, 
                   error:   Message) extends BitFlyerRes

  case class Result(jsonrpc: String,
                    id:      Int, 
                    result:  Boolean) extends BitFlyerRes

  case class Executions(jsonrpc: String,
                        method:  String,
                        params:  Params) extends BitFlyerRes

  case class Message(code:    Int, 
                     message: String)
  case class Params(channel: String,
                    message: Seq[ExecutionInfo])
  case class ExecutionInfo(id:                             Long,
                           side:                           String,
                           price:                          Double,
                           size:                           Double,
                           exec_date:                      String,
                           buy_child_order_acceptance_id:  String,
                           sell_child_order_acceptance_id: String)