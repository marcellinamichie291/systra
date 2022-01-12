package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.util.{Price, isSTOP_LIMIT, isContracted}
import com.github.imomushi8.systra.entity.{Chart, Order, Position}
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.behavior._
import com.github.imomushi8.systra.behavior.Tradable

import cats.implicits._
import cats.effect._
import fs2._

import org.slf4j.{Logger, LoggerFactory}
import com.github.imomushi8.systra.behavior.Flag

/** バックテスト用のロガー */
lazy val globalLogger: Logger = LoggerFactory.getLogger(getClass.getName)

/** Tradableのバックテスト用インスタンス */
object BackTest extends Tradable[BTMarket]:
  val initSub = SummarySubReport(0,0,0,0,0)
  val initSummary = SummaryReport("", 0, 0, 0, OK, initSub, initSub, initSub, 0, 0, 0)
  
  /** SummaryReportをStream上で作成する */
  def makeSummary(traderName: String, firstCapital: Price): Pipe[IO, (PositionReport, Long, Flag, Price), SummaryReport] = _
    .map{ case (record, index, flag, capital) =>
      val pl = record.side*(record.closePrice - record.openPrice)*record.size - record.cost
      (index, flag, record.side, pl, record.cost, capital) /* side, pl, cost */
    }
    .fold((initSummary, 0.0, 0.0, 0, 0, 0.0, OK: Flag)) { 
      case ((currentSummary, currentPL, currentMax, currentSuccWin, currentSuccLose, currentCapital, currentFlag),
            (index, flag, side, pl, cost, capital)) =>
        val nextCapital = currentPL + pl
        val nextMax = currentMax max nextCapital
        val (nextSuccWin, nextSuccLose) = 
          if pl > 0 then (currentSuccWin+1, 0)
          else           (0, currentSuccLose+1)
        val nextSummary = currentSummary ++ (traderName, index, firstCapital, currentCapital, flag, currentPL, currentMax, currentSuccWin, currentSuccLose, side, pl, cost)
        (nextSummary, nextCapital, nextMax, nextSuccWin, nextSuccLose, capital, flag)
    }
    .map(_._1)

def checkAllContract(chart: Chart, orders: List[Order], positions: List[Position]): (List[Order], List[Position], Vector[Report]) =
  val contractedOrders = orders filter isContracted(chart)
  val nonContractedOrders = orders diff contractedOrders
  val nextOrders = getNextOrders(contractedOrders)(nonContractedOrders)

  val normalContractedOrders = contractedOrders filterNot isSTOP_LIMIT
  val closePositions = normalContractedOrders.filter(_.isSettle) >>= settle(chart, positions)
  val openPositions  = normalContractedOrders.filterNot(_.isSettle) map (createPosition(chart))
  val nextPositions  = positions.diff(closePositions.map(_._1)) ++ openPositions
  (nextOrders, nextPositions, makeReport(closePositions))