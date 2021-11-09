package com.github.imomushi8.systra.behavior

import cats.data.{State, Reader}
import cats.Monoid._
import fs2._

import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.MarketContext
import com.github.imomushi8.systra.util.Chart
import com.github.imomushi8.systra.report.Report

trait Tradable[Market](using MarketBehavior[Market]):
  def apply[F[_], Memory](brain: Brain[Market, Memory], 
                          initS: (Market,Memory)): Reader[Stream[F, Chart], Stream[F, Seq[Report]]] = Reader{
    stream =>
      val f = trade(brain)
      stream.scan((initS, Seq[Report]())) {
        case ((current, _), chart) => f(chart).run(current).value
      }
      .map(_._2).foldMonoid
  }

  def contract[Memory]: State[(Market, Memory), Seq[Report]]

  def getContext(market: Market): MarketContext[Market]

  private def updateChart[Memory](chart: Chart): State[(Market, Memory), Unit] =
    State.modify { case (market, memory) => (market := chart, memory) }

  private def action[Memory](chart: Chart, brain: Brain[Market, Memory]): State[(Market, Memory), Unit] =
    State.modify { case (market, memory) => brain(chart, getContext(market), memory) }
  
  def trade[Memory](brain: Brain[Market, Memory])(chart: Chart): State[(Market, Memory), Seq[Report]] = 
    for
      ?    <- updateChart(chart)
      logs <- contract[Memory]
      ?    <- action(chart, brain)
    yield 
      logs