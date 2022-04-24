package com.github.imomushi8.systra.core.util

import cats.implicits._

import io.circe.{Json, Encoder, Decoder, HCursor}
import io.circe.Json._
import io.circe.syntax._
import io.circe.generic.semiauto._

case class JsonRpcRes[R](jsonrpc: String,
                         id:      Int, 
                         result:  Option[R],
                         error:   Option[JsonRpcRes.Message]) extends JsonRpc

object JsonRpcRes:
  given messageEncoder: Encoder[Message] = deriveEncoder
  given messageDecoder: Decoder[Message] = deriveDecoder

  implicit def encoder[R: Encoder]: Encoder[JsonRpcRes[R]] = res => 
    val pairList: Seq[(String, Json)] = List(
      "jsonrpc" -> fromString(res.jsonrpc),
      "id"  -> fromInt(res.id)
    ) ++ res.result.map { "result"  -> _.asJson }.toList
      ++ res.error.map { "error"  -> _.asJson }.toList
      
    obj(pairList*)

  implicit def decoder[R: Decoder]: Decoder[JsonRpcRes[R]] = (c: HCursor) =>
    for {
      jsonrpc <- c.get[String]("jsonrpc")
      id      <- c.get[Int]("id")
      result  <- c.getOrElse[Option[R]]("result")(None)
      error   <- c.getOrElse[Option[Message]]("error")(None)
    } yield JsonRpcRes(jsonrpc, id, result, error)
  
  case class Message(code:    Int, 
                     message: String)