package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.{Price, ID, Size, TimeStamp}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report.{Report, PositionReport}

def checkAllContract(chart: Chart, orders: List[Order], positions: List[Position]): (List[Order], List[Order], List[(Position, Price, TimeStamp)], List[Position]) =
  /* 削除：子注文を除き、約定のイベントが発生している注文 */
  val contractedOrders = orders.filterNot(_.hasParent).filter(isContracted(chart))
  /* 追加：約定済のSTOP_LIMIT注文を、LIMIT注文に変換した注文 */
  val newLimitOrders = contractedOrders.filter(order => order.isSTOP && order.isLIMIT).map(_.invalidateTriggerPrice)
  /* 約定済のLIMITまたはSTOP注文の注文 */
  val normalContractedOrders = contractedOrders.filterNot(order => order.isSTOP && order.isLIMIT)
  /* 削除：約定しているLIMIT,STOP注文のうち、OCO注文と紐づいている注文 */
  val ocoOrders = normalContractedOrders.flatMap { contractedOrder => orders.filter(_.id == contractedOrder.brotherId) }
  /* 削除：約定しているLIMIT,STOP注文のうち、IFD注文と紐づいている注文 */
  val oldIfdOrders = normalContractedOrders.flatMap { contractedOrder => orders.filter(_.parentId == contractedOrder.id) }
  /* 追加：IFD注文と紐づいているもので、parentIdを無効化した注文 */
  val newIfdOrders = oldIfdOrders.map(_.invalidateParentId)
  /* 追加：約定によって開いたポジション */
  val openPositions = normalContractedOrders.filterNot(_.isSettle).map(createPosition(chart))
  /* 削除：約定によって閉じたポジション */
  val closePositions = normalContractedOrders.filter(_.isSettle).flatMap(settle(chart, positions))

  val deleteOrders = contractedOrders ++ (ocoOrders ++ oldIfdOrders)
  val newOrders = newLimitOrders ++ newIfdOrders
  val deletePositions = closePositions
  val newPositions = openPositions

  (deleteOrders, newOrders, deletePositions, newPositions)

/** ポジションを作る */
def createPosition(chart: Chart)(contractOrder: Order): Position =
  val validPrice = if contractOrder.isLIMIT then contractOrder.price else contractOrder.triggerPrice
  Position(chart.datetime, contractOrder.id, contractOrder.side, validPrice, contractOrder.size)

/** ポジションを決済する */
def settle(chart: Chart, positions: List[Position])(contractOrder: Order): List[(Position, Size, TimeStamp)] =
  val validPrice = if contractOrder.price > 0 then contractOrder.price else contractOrder.triggerPrice
  positions.filter(_.id == contractOrder.settlePositionId).map((_, validPrice, chart.datetime))

/** 約定しているかどうか TODO: 見直し（特に高値・安値の部分の反転は正しいか？） */
def isContracted(chart: Chart)(order: Order): Boolean =
  val contracted =
    if order.isSTOP then /* STOP, STOP_LIMITが約定している判定を先に行う */
      order.side * chart.low >= order.side * order.triggerPrice
    else /* LIMIT */
      order.side * chart.high <= order.side * order.price

  order.isMarket || contracted

def makeReport(contractedPositions: List[(Position, Price, TimeStamp)]): Vector[Report] =
  contractedPositions.map { case (position, closePrice, closeTime) =>
    PositionReport(position.openTime,
                   closeTime,
                   position.side,
                   position.size,
                   position.price,
                   closePrice,
                   0)}.toVector
