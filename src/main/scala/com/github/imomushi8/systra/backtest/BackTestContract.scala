package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.{Price, ID, Size, TimeStamp}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report.{Report, PositionReport}

import cats.implicits._

def checkAllContract(chart: Chart, orders: List[Order], positions: List[Position]): (List[Order], List[Position], List[(Position, Price, TimeStamp)]) =
/*
  // 削除：約定しているLIMIT,STOP注文のうち、OCO注文と紐づいている注文
  val ocoOrders = contractedOrders.filterNot(order => order.isSTOP && order.isLIMIT).flatMap { contractedOrder => orders.filter(_.id == contractedOrder.brotherId) }
  // 削除：約定しているLIMIT,STOP注文のうち、IFD注文と紐づいている注文
  val oldIfdOrders = contractedOrders.filterNot(order => order.isSTOP && order.isLIMIT).flatMap { contractedOrder => orders.filter(_.parentId == contractedOrder.id) }
  // 追加：約定済のSTOP_LIMIT注文を、LIMIT注文に変換した注文
  val newLimitOrders = contractedOrders.filter(order => order.isSTOP && order.isLIMIT).map(_.invalidateTriggerPrice)
  // 追加：IFD注文と紐づいているもので、parentIdを無効化した注文
  val newIfdOrders = oldIfdOrders.map(_.invalidateParentId)

  val openOrders  = newLimitOrders ++ newIfdOrders
  val closeOrders = contractedOrders ++ ocoOrders ++ oldIfdOrders
  val nextOrders  = orders.diff(closeOrders) ++ openOrders
*/

  val contractedOrders = orders filter (isContracted(chart))
  val normalContractedOrders = contractedOrders filterNot (isSTOP_LIMIT)

  val nextOrders = orders.diff(contractedOrders) >>= convertOrder(contractedOrders)

  val closePositions = normalContractedOrders.filter(_.isSettle) >>= settle(chart, positions)
  
  val openPositions  = normalContractedOrders.filterNot(_.isSettle).map(createPosition(chart))
  val nextPositions  = positions.diff(closePositions.map(_._1)) ++ openPositions
  (nextOrders, nextPositions, closePositions)

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

/** 未約定注文を用いて、約定判定のある注文リストを追加・変換・削除 */
def convertOrder(contractedOrders: List[Order])(nonContractedOrder: Order): List[Order] = contractedOrders.flatMap { 
  contractedOrder => 
    if isSTOP_LIMIT(contractedOrder) then List(contractedOrder.invalidateTriggerPrice) // STOP_LIMIT注文はLIMIT注文に変換
    else
      if contractedOrder.brotherId == nonContractedOrder.id then Nil // OCO注文なら片方を削除
      else if contractedOrder.id == nonContractedOrder.parentId then List(nonContractedOrder.invalidateParentId) // IFD注文なら親注文IDを削除したものに変換
      else List(nonContractedOrder) // 関係ないものはそのまま
}


/** ポジションを作る TODO: 成り行き注文の分岐を追加 */
def createPosition(chart: Chart)(contractOrder: Order): Position =
  val validPrice = if contractOrder.isLIMIT then contractOrder.price else contractOrder.triggerPrice
  Position(chart.datetime, contractOrder.id, contractOrder.side, validPrice, contractOrder.size)

/** ポジションを決済する */
def settle(chart: Chart, positions: List[Position])(contractOrder: Order): List[(Position, Size, TimeStamp)] =
  val validPrice = if contractOrder.price > 0 then contractOrder.price else contractOrder.triggerPrice
  positions.filter(_.id == contractOrder.settlePositionId).map((_, validPrice, chart.datetime))

/** 決済済みポジションからPositionReportを作成 */
def makeReport(contractedPositions: List[(Position, Price, TimeStamp)]): Vector[Report] = contractedPositions.map { 
  case (Position(openTime, id, side, price, size), closePrice, closeTime) =>
    PositionReport(openTime, closeTime, side, size, price, closePrice, 0)
}.toVector
