package com.github.imomushi8.systra.core.util

import io.circe.{Json, Encoder, Decoder, HCursor}
import io.circe.Json._
import io.circe.syntax._
import io.circe.generic.semiauto._

trait JsonRpc

case class JsonRpcReq[Params](jsonrpc: String="2.0", 
                              method:  String, 
                              params:  Params,
                              id:      Option[Int]=None) extends JsonRpc

object JsonRpcReq:
  implicit def encoder[Params: Encoder]: Encoder[JsonRpcReq[Params]] = req => req.id
    .map { i => obj(
      "jsonrpc" -> fromString(req.jsonrpc),
      "method"  -> fromString(req.method),
      "params"  -> req.params.asJson,
      "id"      -> fromInt(i)
    )}
    .getOrElse { obj(
      "jsonrpc" -> fromString(req.jsonrpc),
      "method"  -> fromString(req.method),
      "params"  -> req.params.asJson,
    )}

  implicit def decoder[Params: Decoder]: Decoder[JsonRpcReq[Params]] = (c: HCursor) =>for
    jsonrpc <- c.get[String]("jsonrpc")
    method  <- c.get[String]("method")
    params  <- c.get[Params]("params")
    id      <- c.get[Int]("id")
  yield JsonRpcReq(jsonrpc, method, params, Some(id))