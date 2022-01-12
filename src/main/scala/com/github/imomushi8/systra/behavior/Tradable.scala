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

trait Flag
case object OK extends Flag
case class  NG(msg: String) extends Flag:
  override val toString: String = "NG"

trait Tradable[Market](using MarketBehavior[Market]):
  def apply[Memory](brain:      Brain[Market, Memory],
                    initMarket: Market,
                    initMemory: Memory): Pipe[IO, Chart, (Vector[Report], Flag, Price)] = {
    val f = trade(brain)
    stream => stream
      .evalScan(((initMarket, initMemory, OK: Flag), Vector[Report]())) { (current, chart) => f(chart).run(current._1) }
      .map { case ((market, memory, flag), logs) => 
        /*
        val context = market.context
        val strOrder=context.orders.headOption.map(o=>s"Order is $o")
        val strPosition=context.positions.headOption.map(p=>s"Positions is $p")
        println(s"Capital is ${context.capital}, AllCapital is ${context.allCapital}, $strOrder, $strPosition")
        */
        (logs, flag, market.context.allCapital) }
  }
  
  def trade[Memory](brain: Brain[Market, Memory])(chart: Chart): StateT[IO, (Market, Memory, Flag), Vector[Report]] = 
    for
      ?       <- updateChart(chart)
      reports <- checkContract[Memory]
      action  <- execAction(chart, brain)
      ?       <- action match
                  case Next(market, memory) => StateT.set[IO, (Market, Memory, Flag)]((market, memory, OK))
                  case End(endMsg)          => StateT.modify[IO, (Market, Memory, Flag)](s=>(s._1, s._2, NG(endMsg)))
    yield reports

  def checkContract[Memory]: StateT[IO, (Market, Memory, Flag), Vector[Report]] = StateT { case (market, memory, flag) => 
    market.contract.map { case (nextMarket, logs) => ((nextMarket, memory, flag), logs.toVector)}
  }

  def updateChart[Memory](chart: Chart): StateT[IO, (Market, Memory, Flag), Unit] = StateT.modify {
    case (market, memory, flag) => (market := chart, memory, flag)
  }

  def execAction[Memory](chart: Chart, brain: Brain[Market, Memory]): StateT[IO, (Market, Memory, Flag), TradeAction[Market, Memory]] = 
    StateT.inspectF { case (market, memory, flag) => flag match
      case OK      => brain(chart, market.context, memory)
      case NG(msg) => IO(End(msg))
    }

  