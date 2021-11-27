package com.github.imomushi8.systra.behavior

import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.{MarketContext, TradeAction, Next, End}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.entity._
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.behavior._

import cats.kernel.Monoid
import cats.data._
import cats.syntax.flatMap.catsSyntaxFlatMapOps
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.unsafe.implicits.global
import fs2._

trait Tradable[Market](using MarketBehavior[Market]):
  def apply[Memory](brain: Brain[Market, Memory]): Chart => State[(Market, Memory), Vector[Report]] = 
    tradeFlow(brain)

  def tradeFlow[Memory](brain: Brain[Market, Memory])(chart: Chart): State[(Market, Memory), Vector[Report]] = 
      for
        ?       <- updateChart(chart)
        reports <- checkContract[Memory]
        action  <- execAction(chart, brain)
        ?       <- action match
                    case Next(market, memory) => State.set((market, memory))
                    case End(endMsg)          => throw new Exception(endMsg) // TODO: なにかしか独自エラークラスを定義すべき？
      yield reports

  def checkContract[Memory]: State[(Market, Memory), Vector[Report]] = State { case (market, memory) => 
    market.contract.map { case (nextMarket, logs) => ((nextMarket, memory), logs.toVector)}.unsafeRunSync()
  }

  def updateChart[Memory](chart: Chart): State[(Market, Memory), Unit] = State.modify {
    case (market, memory) => (market := chart, memory)
  }

  def execAction[Memory](chart: Chart, brain: Brain[Market, Memory]): State[(Market, Memory), TradeAction[Market, Memory]] = 
    State.inspect { case (market, memory) => brain(chart, market.context, memory) }