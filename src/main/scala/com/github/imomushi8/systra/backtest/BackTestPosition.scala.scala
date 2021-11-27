package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.{Price, ID, Size, TimeStamp}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report.{Report, PositionReport}

import cats.implicits._

/** ポジションを作る TODO: 成り行き注文の分岐を追加 */
def createPosition(chart: Chart)(contractedOrder: Order): Position =
  val validPrice = if contractedOrder.isLIMIT then contractedOrder.price else contractedOrder.triggerPrice
  Position(chart.datetime, contractedOrder.id, contractedOrder.side, validPrice, contractedOrder.size)

/** 
 * ポジションを決済する
 * FIXME: 約定判定が甘い。とくに、存在していないIDに対する決済処理はエラーにするべき。
 * TODO: ポジションのすべてを閉じるのではなく、sizeの差分だけ決済する処理が欲しい
 */
def settle(chart: Chart, positions: List[Position])(contractedOrder: Order): List[(Position, Price, TimeStamp)] =
  val validPrice = if contractedOrder.price > 0 then contractedOrder.price else contractedOrder.triggerPrice
  positions.collect {
    case position if position.id == contractedOrder.settlePositionId => (position, validPrice, chart.datetime)
  }

/** 決済済みポジションからPositionReportを作成 */
def makeReport(contractedPositions: List[(Position, Price, TimeStamp)]): Vector[Report] = contractedPositions.map { 
  case (Position(openTime, id, side, price, size), closePrice, closeTime) =>
    PositionReport(openTime, closeTime, side, size, price, closePrice, 0)
}.toVector
