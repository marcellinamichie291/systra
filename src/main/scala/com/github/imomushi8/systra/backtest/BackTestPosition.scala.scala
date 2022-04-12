package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.report.Report

import cats.implicits._

/** ポジションを作る */
def createPosition(chart: Chart)(contractedOrder: Order): Position =
  val validPrice = if contractedOrder.isLIMIT then contractedOrder.price else contractedOrder.triggerPrice
  Position(chart.datetime, contractedOrder.id, contractedOrder.side, validPrice, contractedOrder.size)

/** ポジションを決済する */
def settle(chart: Chart, positions: List[Position])(contractedOrder: Order): List[(Position, Price, TimeStamp)] =
  // FIXME: SELLポジションにSELL注文をぶつけるなどの場合に例外処理が必要
  checkSettlePositionId(positions)(contractedOrder.settlePositionId) // settlePositionIdが存在するかチェック
  val validPrice = if contractedOrder.price > 0 then contractedOrder.price else contractedOrder.triggerPrice
  positions.collect {
    case position if position.id == contractedOrder.settlePositionId => (position, validPrice, chart.datetime)
  }

/** 決済済みポジションからPositionTransactionを作成 */
def makeReport(contractedPositions: List[(Position, Price, TimeStamp)]): Vector[PositionTransaction] = contractedPositions.map { 
  case (Position(openTime, id, side, price, size), closePrice, closeTime) =>
    PositionTransaction(openTime, closeTime, side, size, price, closePrice, 0)
}.toVector
