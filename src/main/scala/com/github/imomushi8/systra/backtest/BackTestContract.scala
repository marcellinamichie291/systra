package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.{Price, ID, Size, TimeStamp}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report.{Report, PositionReport}

import cats.implicits._

def checkAllContract(chart: Chart, orders: List[Order], positions: List[Position]): (List[Order], List[Position], Vector[Report]) =
  val contractedOrders = orders filter isContracted(chart)
  val nonContractedOrders = orders diff contractedOrders

  val nextOrders =
    if contractedOrders.isEmpty then // 約定した注文がなければそのままOrderにしないといけない
      nonContractedOrders
    else 
      contractedOrders >>= convertOrder(nonContractedOrders)

  val normalContractedOrders = contractedOrders filterNot isSTOP_LIMIT
  val closePositions = normalContractedOrders.filter(_.isSettle) >>= settle(chart, positions)
  val openPositions  = normalContractedOrders.filterNot(_.isSettle) map (createPosition(chart))
  val nextPositions  = positions.diff(closePositions.map(_._1)) ++ openPositions
  (nextOrders, nextPositions, makeReport(closePositions))

/** STOP_LIMIT注文かどうかを判定 */
def isSTOP_LIMIT(order: Order) = order.isSTOP && order.isLIMIT

/* STOP, STOP_LIMITが約定している判定を先に行う */
def hasContractEvent(chart: Chart, order: Order): Boolean =
  if order.isSTOP then 
    val thresPrice = if order.isBUY then chart.high else chart.low
    order.side * thresPrice > order.side * order.triggerPrice
  else 
    val thresPrice = if order.isBUY then chart.low else chart.high
    order.side * thresPrice < order.side * order.price

/** 
 * 約定しているかどうかの判定 
 * 親注文を持っていない　かつ（成り行き注文　または　約定イベントが発生している注文）
 */
def isContracted(chart: Chart)(order: Order): Boolean = 
  !order.hasParent && (order.isMarket || hasContractEvent(chart, order))

/** 約定注文を用いて、未約定注文リストから関係のあるものを追加・変換・削除 */
def convertOrder(nonContractedOrders: List[Order])(contractedOrder: Order): List[Order] =
  if isSTOP_LIMIT(contractedOrder) then 
    List(contractedOrder.invalidateTriggerPrice) // STOP_LIMIT注文はLIMIT注文に変換
  else
    nonContractedOrders.flatMap { nonContractedOrder =>    
      if contractedOrder.id == nonContractedOrder.parentId then List(nonContractedOrder.invalidateParentId) // IFD注文なら親注文IDを削除したものに変換
      else if contractedOrder.id == nonContractedOrder.brotherId then Nil // OCO注文なら片方を削除
      else List(nonContractedOrder) // 関係ないものはそのまま
    }

/** ポジションを作る TODO: 成り行き注文の分岐を追加 */
def createPosition(chart: Chart)(contractedOrder: Order): Position =
  val validPrice = if contractedOrder.isLIMIT then contractedOrder.price else contractedOrder.triggerPrice
  Position(chart.datetime, contractedOrder.id, contractedOrder.side, validPrice, contractedOrder.size)

/** ポジションを決済する TODO: 成り行き注文の分岐を追加 */
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
