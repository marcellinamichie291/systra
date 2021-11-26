package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.{Price, ID, Size, TimeStamp}
import com.github.imomushi8.systra.util._

/**
 * 発注処理を行う
 * TODO: 理論的に不可能な注文をはじくように実装する
 */
def makeOrder(chart  :Chart,
              id     :Int, 
              method :OrderMethod, 
              size   :Size, 
              expire :TimeStamp): List[Order] = {
  import scala.language.implicitConversions
  implicit def toStr(i: Int): String = i.toString

  /** 単一注文をOrderにする */
  def makeChildOrder(childId     :ID,
                     childMethod :ChildOrderMethod,
                     parentId    :ID,
                     brotherId   :ID): Order = childMethod match
    case      LIMIT(side, price,               positionId) => Order(childId, side, price,            0, size, expire, positionId, parentId, brotherId)
    case STOP      (side,        triggerPrice, positionId) => Order(childId, side,     0, triggerPrice, size, expire, positionId, parentId, brotherId)
    case STOP_LIMIT(side, price, triggerPrice, positionId) => Order(childId, side, price, triggerPrice, size, expire, positionId, parentId, brotherId)

  method match
    case MARKET(side, positionId) => List(
      Order(id, side, chart.close, 0, size, expire, positionId, "", "", isMarket = true))
    case OCO(upperMethod, lowerMethod) => List(
      makeChildOrder(id    , upperMethod, "", id + 1),
      makeChildOrder(id + 1, lowerMethod, "", id    ))
    case IFD(parentMethod, childMethod) => List(
      makeChildOrder(id    , parentMethod, "", ""),
      makeChildOrder(id + 1,  childMethod, id, ""))
    case IFDOCO(parentMethod, upperMethod, lowerMethod) => List(
      makeChildOrder(id    , parentMethod, "",     ""),
      makeChildOrder(id + 1,  upperMethod, id, id + 2),
      makeChildOrder(id + 2,  lowerMethod, id, id + 1))
    case method: ChildOrderMethod => List(
      makeChildOrder(id, method, "", ""))
}