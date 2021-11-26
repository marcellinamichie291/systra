package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.{Price, ID, Size, TimeStamp}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report.{Report, PositionReport}

import cats.implicits._

def checkAllContract(chart: Chart, orders: List[Order], positions: List[Position]): (List[Order], List[Position], Vector[Report]) =
  val contractedOrders = orders filter isContracted(chart)
  val nonContractedOrders = orders diff contractedOrders
  val nextOrders = getNextOrders(contractedOrders)(nonContractedOrders)

  val normalContractedOrders = contractedOrders filterNot isSTOP_LIMIT
  val closePositions = normalContractedOrders.filter(_.isSettle) >>= settle(chart, positions)
  val openPositions  = normalContractedOrders.filterNot(_.isSettle) map (createPosition(chart))
  val nextPositions  = positions.diff(closePositions.map(_._1)) ++ openPositions
  (nextOrders, nextPositions, makeReport(closePositions))

/** STOP_LIMIT注文かどうかを判定 */
def isSTOP_LIMIT(order: Order): Boolean = order.isSTOP && order.isLIMIT

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
  
/** 約定注文と関連するものを取得する */
def getRelated(nonContractedOrders: List[Order])(contractedOrder: Order): List[Order] = 
  nonContractedOrders filter { nonContractedOrder =>
    contractedOrder.id == nonContractedOrder.parentId || contractedOrder.id == nonContractedOrder.brotherId
  }

/** 約定注文とその関連注文を変換する */
def convertOrders(relatedOrders: List[Order])(contractedOrder: Order): List[Order] =
  // STOP_LIMIT注文はLIMIT注文に変換
  if isSTOP_LIMIT(contractedOrder) then
    List(contractedOrder.invalidateTriggerPrice) 
  // 残りは関連する注文に変換
  else
    relatedOrders collect {
      // IFD注文なら親注文IDを削除したものに変換
      case relatedOrder if contractedOrder.id == relatedOrder.parentId => relatedOrder.invalidateParentId 
      //OCO注文は削除
    }

/** 次の注文を取得する */
def getNextOrders(contractedOrders: List[Order])(nonContractedOrders: List[Order]): List[Order] = 
  val relatedOrders    = (contractedOrders flatMap getRelated(nonContractedOrders)).distinct // 関連注文は2重に取得できる可能性があるので、distinctを使用
  val convertedOrders  = contractedOrders flatMap convertOrders(relatedOrders)
  val nonRelatedOrders = nonContractedOrders diff relatedOrders
  convertedOrders ++ nonRelatedOrders


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
