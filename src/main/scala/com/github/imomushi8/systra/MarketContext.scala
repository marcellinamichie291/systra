package com.github.imomushi8.systra

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.Actions._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.behavior.MarketBehavior

import cats.effect.{ExitCode, IO}
import cats.data.EitherT
import cats.implicits.catsSyntaxFlatMapOps

case class MarketContext[Market](capital   :Double,
                                 orders    :List[Order],
                                 positions :List[Position],
                                 private[systra] var market :Market)
                                 
extension [Market](context: MarketContext[Market])
  def placeOrder(method: OrderMethod, size: Size, expire: TimeStamp)
                (using m: MarketBehavior[Market]): TradeIO[ID] = EitherT { 
    (for
      res <- context.market << (method, size, expire)
    yield
      context.market = res._1
      res._2
    ).attempt }

  def cancelOrder(id: ID)(using m: MarketBehavior[Market]): TradeIO[ExitCode] = EitherT { 
    (for
      res <- context.market >> id
    yield
      context.market = res._1
      res._2
  ).attempt }