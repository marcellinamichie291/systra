package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.{Price, ID, Size, TimeStamp}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report.{Report, PositionReport}

import scala.language.implicitConversions

object BackTestProcedure {
  /*----------------------------------------------------------------------------------------------*/
  /* private メソッド */
  /* 注文取得         */

  /** TradeSystemから受け取った注文を開く */
  private[backtest] def getOrders(chart: Chart, id: Int, method: OrderMethod, size: Size, expire: TimeStamp): List[Order] =
    method match {
      /* OCO */
      case OCO(upperMethod, lowerMethod) => List(
        makeOrder(chart, id, upperMethod, size, expire, "", id + 1),
        makeOrder(chart, id + 1, lowerMethod, size, expire, "", id))
      /* IFD */
      case IFD(parentMethod, childMethod) => List(
        makeOrder(chart, id, parentMethod, size, expire, "", ""),
        makeOrder(chart, id + 1, childMethod, size, expire, id.toString, ""))
      /* IFDOCO */
      case IFDOCO(parentMethod, upperMethod, lowerMethod) => List(
        makeOrder(chart, id, parentMethod, size, expire, "", ""),
        makeOrder(chart, id + 1, upperMethod, size, expire, id, id + 2),
        makeOrder(chart, id + 2, lowerMethod, size, expire, id, id + 1))
      /* トレール */
      case TRAIL() => Nil
      /* 単一注文の場合 */
      case method =>
        List(makeOrder(chart, id, method, size, expire, "", ""))
    }

  /** 子注文をOrderにする */
  private def makeOrder(chart: Chart,
                        id: ID,
                        method: OrderMethod,
                        size: Size,
                        expire: TimeStamp,
                        parentId: ID,
                        brotherId: ID): Order = method match {
    /* 成行 */
    case MARKET(side, positionId) =>
      Order(id, side, chart.close, 0, size, expire, positionId, parentId, brotherId, isMarket = true)
    /* 指値 */
    case LIMIT(side, price, positionId) =>
      Order(id, side, price, 0, size, expire, positionId, parentId, brotherId)
    /* 逆指値（成行） */
    case STOP(side, triggerPrice, positionId) =>
      Order(id, side, 0, triggerPrice, size, expire, positionId, parentId, brotherId)
    /* 逆指値（指値） */
    case STOP_LIMIT(side, price, triggerPrice, positionId) =>
      Order(id, side, price, triggerPrice, size, expire, positionId, parentId, brotherId)
    case _ => throw new Exception("Not ChildOrder")
  }


  /*----------------------------------------------------------------------------------------------*/
  /* 約定確認 */

  /** すべてのオーダーの約定を確認する */
  private[backtest] def checkAllContract(chart: Chart, orders: List[Order], positions: List[Position])
  : (List[Order], List[Order], List[(Position, Size, TimeStamp)], List[Position]) = {
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
  }

  /** ポジションを作る */
  private def createPosition(chart: Chart)(contractOrder: Order): Position = {
    val validPrice = if (contractOrder.isLIMIT) contractOrder.price else contractOrder.triggerPrice
    Position(chart.datetime, contractOrder.id, contractOrder.side, validPrice, contractOrder.size)
  }

  /** ポジションを決済する */
  private def settle(chart: Chart,
                     positions: List[Position])
                    (contractOrder: Order): List[(Position, Size, TimeStamp)] = {
    val validPrice = if (contractOrder.price > 0) contractOrder.price else contractOrder.triggerPrice
    positions.filter(_.id == contractOrder.settlePositionId).map((_, validPrice, chart.datetime))
  }

  /** 約定しているかどうか TODO: 見直し（特に高値・安値の部分の反転は正しいか？） */
  private def isContracted(chart: Chart)(order: Order): Boolean = {
    val contracted =
    /* STOP, STOP_LIMITが約定している判定を先に行う */
      if (order.isSTOP) {
        order.side * chart.low >= order.side * order.triggerPrice
        /* LIMIT */
      } else {
        order.side * chart.high <= order.side * order.price
      }

    order.isMarket || contracted
  }

  private[backtest] def makeReport(contractedPositions: List[(Position, Price, TimeStamp)]): Vector[Report] =
    contractedPositions.map { case (position, closePrice, closeTime) =>
      PositionReport(position.openTime,
                    closeTime,
                    position.side,
                    position.size,
                    position.price,
                    closePrice,
                    0)
    }.toVector

  implicit def toStr(i: Int): String = i.toString
}
