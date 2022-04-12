package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.market.{Tradable, OK, TradeState}

import cats.implicits._
import cats.effect._
import fs2._

import org.slf4j.{Logger, LoggerFactory}

/** バックテスト用のロガー */
lazy val globalLogger: Logger = LoggerFactory.getLogger(getClass.getName)

/** Tradableのバックテスト用インスタンス */
object BackTest extends Tradable[BTMarket]:
  val initSub = SummarySubReport(0,0,0,0,0)
  val initSummary = SummaryReport("", 0, 0, 0, OK, initSub, initSub, initSub, 0, 0, 0)

  def make[Memory](traderName: String, firstCapital: Price): Pipe[IO, (BTMarket, Memory, TradeState), SummaryReport] = _
    .fold(initSummary) { 
      case (current, (_, _, TradeState(sampleSize, flag, buy, sell, capital, maxCapital, consWins, consLoses, _))) =>
        val buySub = SummarySubReport(buy.winCount, buy.loseCount, buy.profit, buy.loss, buy.cost)
        val sellSub = SummarySubReport(sell.winCount, sell.loseCount, sell.profit, sell.loss, sell.cost)
        val maximalDrawDown = current.maximalDrawDown max (maxCapital - capital)
        val consecutiveWinCount  = current.consecutiveWinCount max consWins
        val consecutiveLoseCount = current.consecutiveLoseCount max consLoses
        SummaryReport(traderName, sampleSize, firstCapital, capital, flag, buySub, sellSub, buySub+sellSub, maximalDrawDown, consecutiveWinCount, consecutiveLoseCount)
    }

    
/** 約定チェック用のグローバル関数 */
def checkAllContract(chart: Chart, orders: List[Order], positions: List[Position]): (List[Order], List[Position], Vector[PositionTransaction]) =
  val contractedOrders = orders filter isContracted(chart)
  val nonContractedOrders = orders diff contractedOrders
  val nextOrders = getNextOrders(contractedOrders)(nonContractedOrders)

  val normalContractedOrders = contractedOrders filterNot isSTOP_LIMIT
  val closePositions = normalContractedOrders.filter(_.isSettle) >>= settle(chart, positions)
  val openPositions  = normalContractedOrders.filterNot(_.isSettle) map (createPosition(chart))
  val nextPositions  = positions.diff(closePositions.map(_._1)) ++ openPositions
  (nextOrders, nextPositions, makeReport(closePositions))