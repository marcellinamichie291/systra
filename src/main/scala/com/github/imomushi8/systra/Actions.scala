package com.github.imomushi8.systra

import com.github.imomushi8.systra.util.Chart
import cats.data.EitherT
import cats.implicits.catsSyntaxApplicativeId 
import cats.syntax.all.catsSyntaxApplicativeId
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.effect.IO
import org.atnos.eff._
import org.atnos.eff.syntax.eff._

object Actions {
  type Brain[Market, Memory] = (Chart, MarketContext[Market], Memory) => TradeAction[Market, Memory]
  type TradeIO = [A] =>> EitherT[IO, Throwable, A]

  def receive[Market, Memory](brain: Brain[Market, Memory]): Brain[Market, Memory] = brain

  def next[Market, Memory](memory: Memory, context: MarketContext[Market]): TradeAction[Market, Memory] =
    Next(context.market, memory)

  def end[Market, Memory](t: Throwable): TradeAction[Market, Memory] = End(t)

  def io[A](a: A): TradeIO[A] = EitherT.pure[IO, Throwable](a)
}