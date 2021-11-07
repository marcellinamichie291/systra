package com.github.imomushi8.systra

import cats.effect.{ExitCode, IO}
import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.behavior.MarketBehavior

case class MarketContext[Market](capital   :Double,
                                 orders    :List[Order],
                                 positions :List[Position],
                                 private[systra] var market :Market)

extension [Market](context: MarketContext[Market])

  def placeOrder(method: OrderMethod,
                 size: Size,
                 expire: TimeStamp)
                (using m: MarketBehavior[Market]): IO[ID] = for {
    res <- m.placeOrder(context.market, method, size, expire)
  } yield {
    context.market = res._1
    res._2
  }

  def cancelOrder(id: ID)
                 (using m: MarketBehavior[Market]): IO[ExitCode] = for {
    res <- m.cancelOrder(context.market, id)
  } yield {
    context.market = res._1
    res._2
  }