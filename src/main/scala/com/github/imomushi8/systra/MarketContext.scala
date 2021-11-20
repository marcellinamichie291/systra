package com.github.imomushi8.systra

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.Actions._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.behavior.MarketBehavior

import cats.effect.{ExitCode, IO}
import cats.effect.kernel.Ref
import cats.data.{State, StateT, EitherT}
import cats.implicits.catsSyntaxFlatMapOps
import cats.effect.unsafe.implicits.global


case class MarketContext[Market](capital   :Double,
                                 orders    :List[Order],
                                 positions :List[Position],
                                 market    :Market)
                                 
extension [Market](context: MarketContext[Market])

  def placeOrder(method: OrderMethod, size: Size, expire: TimeStamp)
                (using m: MarketBehavior[Market]): StateT[IO, Market, ID] =
    StateT { (current: Market) => (current << (method, size, expire)) }

  def cancelOrder(id: ID)(using m: MarketBehavior[Market]): StateT[IO, Market, ExitCode] = StateT {
    (current: Market) => (current >> id)
  }