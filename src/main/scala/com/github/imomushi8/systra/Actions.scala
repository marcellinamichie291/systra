package com.github.imomushi8.systra

import com.github.imomushi8.systra.util.Chart
import cats.data.{State, EitherT, StateT}
import cats.implicits.catsSyntaxApplicativeId
import cats.effect.unsafe.implicits.global
import cats.effect.IO
import cats.effect.kernel.Ref

object Actions {
  type Brain[Market, Memory] = (Chart, MarketContext[Market], Memory) => TradeAction[Market, Memory]

  def receive[Market, Memory](brain: Brain[Market, Memory]): Brain[Market, Memory] = brain

  /** 一度でもエラーが発生したらBrainを終了させる */
  def nextHandleErrorWith[Market, Memory, A](memory: Memory, 
                                             context: MarketContext[Market], 
                                             refIO: IO[Ref[IO, Market]]): TradeAction[Market, Memory] = 
    refIO.flatMap(_.get).attempt.unsafeRunSync() match
      case Right(next)     => Next(next, memory)
      case Left(throwable) => End(throwable.getMessage)
    
      
  /** エラー処理はBrain側でやらせるので、この関数は必ずNextを返す */
  def next[Market, Memory, A](memory: Memory, 
                              context: MarketContext[Market], 
                              refOp: Option[Ref[IO, Market]] = None): TradeAction[Market, Memory] = refOp match
    case Some(refMarket) => Next(refMarket.get.unsafeRunSync(), memory)
    case None            => Next(context.market, memory)

  def end[Market, Memory](endMsg: String): TradeAction[Market, Memory] = End(endMsg)
}