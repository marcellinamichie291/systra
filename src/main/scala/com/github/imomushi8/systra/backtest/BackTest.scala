package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.util.{isSTOP_LIMIT, isContracted}
import com.github.imomushi8.systra.entity.{Chart, Order, Position}
import com.github.imomushi8.systra.report.Report
import com.github.imomushi8.systra.behavior.Tradable

import cats.implicits._

/** Tradableのバックテスト用インスタンス */
object BackTest extends Tradable[BTMarket]

def checkAllContract(chart: Chart, orders: List[Order], positions: List[Position]): (List[Order], List[Position], Vector[Report]) =
  val contractedOrders = orders filter isContracted(chart)
  val nonContractedOrders = orders diff contractedOrders
  val nextOrders = getNextOrders(contractedOrders)(nonContractedOrders)

  val normalContractedOrders = contractedOrders filterNot isSTOP_LIMIT
  val closePositions = normalContractedOrders.filter(_.isSettle) >>= settle(chart, positions)
  val openPositions  = normalContractedOrders.filterNot(_.isSettle) map (createPosition(chart))
  val nextPositions  = positions.diff(closePositions.map(_._1)) ++ openPositions
  (nextOrders, nextPositions, makeReport(closePositions))