package com.github.imomushi8.systra.behavior

import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.{MarketContext, TradeAction, Next, End}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.entity._
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.behavior._

import cats.kernel.Monoid
import cats.data._
import cats.data.StateT
import cats.syntax.flatMap.catsSyntaxFlatMapOps
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.unsafe.implicits.global
import fs2._

trait Tradable[Market](using MarketBehavior[Market]):
  def apply[Memory](trade      : Chart => StateT[IO, (Market, Memory), Vector[Report]],
                    initMarket : Market, 
                    initMemory : Memory): Pipe[IO, Chart, Vector[Report]] = _
    .evalScan(((initMarket, initMemory), Vector[Report]())) { (current, chart) => trade(chart).run(current._1) }
    .map(_._2)
  
  def trade[Memory](brain: Brain[Market, Memory]): Chart =>StateT[IO, (Market, Memory), Vector[Report]] = tradeFlow(brain)

  def tradeFlow[Memory](brain: Brain[Market, Memory])(chart: Chart): StateT[IO, (Market, Memory), Vector[Report]] = 
      for
        ?       <- updateChart(chart)
        reports <- checkContract[Memory]
        action  <- execAction(chart, brain)
        ?       <- action match
                    case Next(market, memory) => StateT.set[IO, (Market, Memory)]((market, memory))
                    case End(endMsg)          => StateT.liftF(IO(throw new Exception(endMsg))) // TODO: なにかしか独自エラークラスを定義すべき？
      yield reports

  def checkContract[Memory]: StateT[IO, (Market, Memory), Vector[Report]] = StateT { case (market, memory) => 
    market.contract.map { case (nextMarket, logs) => ((nextMarket, memory), logs.toVector)}
  }

  def updateChart[Memory](chart: Chart): StateT[IO, (Market, Memory), Unit] = StateT.modify {
    case (market, memory) => (market := chart, memory)
  }

  def execAction[Memory](chart: Chart, brain: Brain[Market, Memory]): StateT[IO, (Market, Memory), TradeAction[Market, Memory]] = 
    StateT.inspectF { case (market, memory) => brain(chart, market.context, memory) }