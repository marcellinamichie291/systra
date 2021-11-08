package com.github.imomushi8.systra

import com.github.imomushi8.systra.util.Chart
import cats.data.EitherT
import cats.implicits.catsSyntaxApplicativeId 
import cats.syntax.all.catsSyntaxApplicativeId
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.effect.IO

object Actions {
  type Brain = [Market, Memory] =>> (Chart, MarketContext[Market], Memory) => (Market, Memory)
  type TradeIO = [A] =>> EitherT[IO, Throwable, A]

  def receive[Market, Memory](brain: Brain[Market, Memory]): Brain[Market, Memory] = brain

  def next[Market, Memory](memory: Memory, context: MarketContext[Market]): (Market, Memory) =
    (context.market, memory)

  def io[A](a: A): TradeIO[A] = EitherT.pure[IO, Throwable](a)
}