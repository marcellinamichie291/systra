package com.github.imomushi8.systra.behavior

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.MarketContext
import com.github.imomushi8.systra.util.Chart
import com.github.imomushi8.systra.report.Report

import cats.data.{State, StateT, Reader}
import cats.Monoid._
import cats.implicits._
import fs2._

trait Tradable2[Market](using MarketBehavior[Market]):
  
  type TradeEither[A] = Either[Throwable, A]
  type TradeState[Market, Memory, A] = StateT[TradeEither, (Market, Memory), A]

  private def action[Memory](chart: Chart, brain: Brain[Market, Memory]): TradeState[Market, Memory, Unit] = 
    StateT { case (market, memory) => 
      brain(chart, getContext(market), memory) match {
        case Next(market, memory) => ((market, memory), ()).asRight[Throwable]
        case End(t)               => t.asLeft[((Market, Memory), Unit)]
      }
    }

  def contract[Memory]: TradeState[Market, Memory, Seq[Report]]

  def getContext(market: Market): MarketContext[Market]

  private def updateChart[Memory](chart: Chart): TradeState[Market, Memory, Unit] =
    StateT.modify { case (market, memory) => (market := chart, memory) }
  
  def trade[Memory](brain: Brain[Market, Memory])(chart: Chart): TradeState[Market, Memory, Seq[Report]] = 
    for
      ?    <- updateChart(chart)
      logs <- contract[Memory]
      ?    <- action(chart, brain)
    yield 
      logs