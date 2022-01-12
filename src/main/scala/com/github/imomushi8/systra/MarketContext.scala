package com.github.imomushi8.systra

import com.github.imomushi8.systra.entity._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.behavior.MarketBehavior

import cats.effect.{ExitCode, IO}
import cats.effect.kernel.Ref
import cats.data.{State, StateT, EitherT}
import cats.implicits.catsSyntaxFlatMapOps
import cats.effect.unsafe.implicits.global


case class MarketContext[Market](capital:    Price,
                                 allCapital: Price, 
                                 orders:     List[Order],
                                 positions:  List[Position],
                                 market:     Market)
                                 
extension [Market](context: MarketContext[Market])
  def getMarket: IO[Ref[IO, Market]] = Ref.of(context.market)

extension [Market](refMarket: Ref[IO, Market])
  def placeOrder(method: OrderMethod, size: Size, expire: TimeStamp)
                (using m: MarketBehavior[Market]): IO[ID] =
    for
      market <- refMarket.get 
      res <- market << (method, size, expire)
      _ <- refMarket.set(res._1)
    yield res._2

  def cancelOrder(id: ID)(using m: MarketBehavior[Market]): IO[ExitCode] =
    for
      market <- refMarket.get 
      res <- market >> (id)
      _ <- refMarket.set(res._1)
    yield res._2
