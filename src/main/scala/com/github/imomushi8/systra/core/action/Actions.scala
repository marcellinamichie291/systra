package com.github.imomushi8.systra.core.action

import com.github.imomushi8.systra.core.entity.Chart
import com.github.imomushi8.systra.core.market.MarketContext

import cats.data.{State, EitherT, StateT}
import cats.implicits.catsSyntaxApplicativeId
import cats.effect.unsafe.implicits.global
import cats.effect.IO
import cats.effect.kernel.Ref

type Brain[Market, Memory] = (Chart, MarketContext[Market], Memory) => IO[TradeAction[Market, Memory]]

object Actions {
  def receive[Market, Memory](brain: Brain[Market, Memory]): Brain[Market, Memory] = brain

  /** 一度でもエラーが発生したらBrainを終了させる */
  def nextHandleErrorWith[Market, Memory, A](memory: Memory,
                                             refIO: IO[Ref[IO, Market]]): IO[TradeAction[Market, Memory]] = 
    refIO.attempt.flatMap {
      case Right(ref)     => ref.get.map(Next(_, memory))
      case Left(throwable) => IO(End(throwable.getMessage))
    }

  /** エラー処理はBrain側でやらせるので、この関数は必ずNextを返す */
  def next[Market, Memory, A](memory: Memory, 
                              context: MarketContext[Market], 
                              refOp: Option[Ref[IO, Market]] = None): IO[TradeAction[Market, Memory]] = refOp match
    case Some(refMarket) => refMarket.get.map(Next(_, memory))
    case None            => IO(Next(context.market, memory))

  def end[Market, Memory](endMsg: String): TradeAction[Market, Memory] = End(endMsg)
}