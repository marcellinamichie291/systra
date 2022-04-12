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
import cats.syntax.UniteOps
import com.github.imomushi8.systra.backtest.BTMarket

trait Flag
case object OK extends Flag
case class  NG(msg: String) extends Flag:
  override val toString: String = "NG"

trait Tradable[Market](using MarketBehavior[Market]):
  def apply[Memory: Initial](brain: Brain[Market, Memory])
                             (using im: Initial[Market]): Pipe[IO, Chart, (Market, Memory, TradeState)] = {
    val f = trade(brain)
    val initMarket = im.empty()
    val initState = TradeState.initial(initMarket.context.capital).empty()
    val init = (initMarket, Initial[Memory](), initState)
    stream => stream.evalScan(init) { case (current, chart) => f(chart).runS(current) }
  }

  def trade[Memory](brain: Brain[Market, Memory])(chart: Chart): StateT[IO, (Market, Memory, TradeState), Vector[PositionReport]] = 
    for
      ?            <- updateChart(chart)
      transactions <- checkContract[Memory]
      action       <- execAction(chart, brain)
      ?            <- updateFlag(action)
      ?            <- updateTradeState(transactions)
    yield transactions

  private def updateChart[Memory](chart: Chart): StateT[IO, (Market, Memory, TradeState), Unit] = StateT.modify {
    case (market, memory, state) => (market := chart, memory, state.copy(sampleSize=state.sampleSize+1))
  }
  
  private def checkContract[Memory]: StateT[IO, (Market, Memory, TradeState), Vector[PositionReport]] = StateT { 
    case (market, memory, state) => 
      market.contract.map { case (nextMarket, transactions) => ((nextMarket, memory, state), transactions.toVector) }
  }

  private def execAction[Memory](chart: Chart, brain: Brain[Market, Memory]): StateT[IO, (Market, Memory, TradeState), TradeAction[Market, Memory]] = StateT.inspectF { 
    case (market, memory, state) => state.flag match
      case OK      => brain(chart, market.context, memory)
      case NG(msg) => IO(End(msg))
  }

  private def updateFlag[Memory](action: TradeAction[Market, Memory]): StateT[IO, (Market, Memory, TradeState), Unit] = StateT.modify { 
    case (currentMarket, currentMemory, currentState) =>
      action match
          case Next(nextMarket, nextMemory) => (   nextMarket,    nextMemory, currentState.copy(flag=OK))
          case End(endMsg)                  => (currentMarket, currentMemory, currentState.copy(flag=NG(endMsg)))
  }

  private def updateTradeState[Memory](transactions: Vector[PositionReport]): StateT[IO, (Market, Memory, TradeState), Unit] = StateT.modify { 
    case (currentMarket, currentMemory, currentState) =>
    val init = (currentState.buyRecord, currentState.sellRecord, currentState.capital, currentState.maxCapital, currentState.consecutiveWins, currentState.consecutiveLoses)
    
    val sortedTransactions = transactions.sortBy(_.openTime)
    val (buyRecord, sellRecord, capital, maxCapital, consWin, consLose) = 
      sortedTransactions
        .foldLeft(init) {
          case ((currentBuyRecord, currentSellRecord, currentCapital, currentMax, currentConsWin, currentConsLose),
                PositionReport(_, _, side, size, openPrice, closePrice, cost)) =>
            val pl          = side*(closePrice-openPrice)*size + cost
            val nextCapital = currentCapital + pl
            val nextMax     = currentMax max nextCapital
            val (nextConsWin, nextConsLose) = if pl > 0 then (currentConsWin+1,  0) else (0, currentConsLose+1)
            val (nextBuyRecord, nextSellRecord) =
              if side == BUY then (currentBuyRecord += (pl, cost), currentSellRecord)
              else                (currentBuyRecord, currentSellRecord += (pl, cost))

            (nextBuyRecord, nextSellRecord, nextCapital, nextMax, nextConsWin, nextConsLose)
        }

    val nextState = TradeState(currentState.sampleSize, currentState.flag, buyRecord, sellRecord, capital, maxCapital, consWin, consLose, sortedTransactions)
    (currentMarket, currentMemory, nextState)
  }