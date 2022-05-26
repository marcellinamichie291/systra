package com.github.imomushi8.systra.core.market

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._

import cats.effect.{ExitCode, IO}
import cats.effect.kernel.Ref
import cats.data.{State, StateT, EitherT}
import cats.implicits.catsSyntaxFlatMapOps
import cats.effect.unsafe.implicits.global

/**
 * Marketから受け取ることのできる情報（コンテキスト）
 * @param capital
 *   円資産の価格
 * @param allCapital
 *   全資産の円換算の価格
 * @param orders
 *   現在の注文の一覧
 * @param positions
 *   現在のポジションの一覧
 * @param market
 *   Marketのインスタンス
 */
case class MarketContext[Market](
    capital: Price,
    allCapital: Price,
    orders: List[Order],
    positions: List[Position],
    market: Market)

extension [Market](context: MarketContext[Market]) 
  def getMarket: IO[Ref[IO, Market]] = Ref.of(context.market)

extension [Market](refMarket: Ref[IO, Market])
  def placeOrder(method:  OrderMethod, 
                 size:    Size,
                 expire:  TimeStamp)
                (using m: MarketBehavior[Market]): IO[ID] = for
    market <- refMarket.get
    res    <- market << (method, size, expire)
    ?      <- refMarket.set(res._1)
  yield res._2

  def cancelOrder(id:      ID)
                 (using m: MarketBehavior[Market]): IO[ExitCode] = for
    market <- refMarket.get
    res    <- market >> id
    ?      <- refMarket.set(res._1)
  yield res._2
