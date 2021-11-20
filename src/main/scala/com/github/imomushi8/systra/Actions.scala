package com.github.imomushi8.systra

import com.github.imomushi8.systra.util.Chart
import cats.data.{State, EitherT}
import cats.implicits.catsSyntaxApplicativeId
import cats.syntax.all.catsSyntaxApplicativeId
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.effect.unsafe.implicits.global
import cats.effect.IO
import org.atnos.eff._
import org.atnos.eff.syntax.eff._

object Actions {
  type Brain[Market, Memory] = (Chart, MarketContext[Market], Memory) => TradeAction[Market, Memory]
  type TradeState = [Market, A] =>> EitherT[State[Market, _], Throwable, A]

  def receive[Market, Memory](brain: Brain[Market, Memory]): Brain[Market, Memory] = brain

  /** 一度でもエラーが発生したらBrainを終了させる */
  def nextHandleErrorWith[Market, Memory, A](memory: Memory, 
                                             context: MarketContext[Market], 
                                             stateOp: Option[IO[Market => IO[(Market, A)]]] = None): TradeAction[Market, Memory] = stateOp match
    case Some(io) =>
      val func = io.unsafeRunSync()
      val either = func(context.market).attempt.unsafeRunSync()
      either match {
        case Right((next, _)) => Next(next, memory)
        case Left(t)  => End(t.getMessage)
      }

    case None => Next(context.market, memory)

  /** エラー処理はBrain側でやらせる */
  def next[Market, Memory, A](memory: Memory, 
                              context: MarketContext[Market], 
                              stateOp: Option[Market => (Market, A)] = None): TradeAction[Market, Memory] = stateOp match
    case Some(func) => Next(func(context.market)._1, memory)
    case None       => Next(context.market, memory)

  def end[Market, Memory](endMsg: String): TradeAction[Market, Memory] = End(endMsg)

  def io[Market, A](a: A): TradeState[Market, A] = EitherT.pure[State[Market, _], Throwable](a)
}