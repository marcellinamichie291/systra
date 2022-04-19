package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.backtest.BackTestPosition._
import com.github.imomushi8.systra.backtest.BackTestContract._

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._

import scala.language.implicitConversions
import com.typesafe.scalalogging.LazyLogging

object BackTestOrder extends LazyLogging:
  implicit def toStr(i: Int): String = i.toString

  /** settlePositionIdが存在しなかったら例外を投げる */
  def checkSettlePositionId(positions: List[Position])(positionId: ID) = 
    if positionId.nonEmpty && !positions.exists(_.id == positionId) then
      val errMsg = s"""Settle position is not exist. Maybe already settled.
                      |settle position ID : $positionId""".stripMargin('|')
      logger.warn(errMsg)
      throw new NoSettlePositionException(errMsg)

  /**
   * 発注処理を行う
   * 既に存在しているポジションの中にsettlePositionIdが存在しなければ例外を出す
   * また、気配値を超えた注文を出す場合は注文方法に応じてMAKET,LIMIT等に変更する
   * 
   * TODO: 理論的に不可能な注文をはじくように実装する
   */
  def makeOrder(chart     :Chart,
                positions :List[Position],
                id        :Int, 
                method    :OrderMethod, 
                size      :Size, 
                expire    :TimeStamp): List[Order] = {
    
    lazy val check = checkSettlePositionId(positions)

    /** 単一注文をOrderにする */
    def makeChildOrder(childId     :ID,
                       childMethod :ChildOrderMethod,
                       parentId    :ID,
                       brotherId   :ID): Order = childMethod match
      case LIMIT(side, price, positionId) => 
        check(positionId)
        val (validPrice, isMarket) = // 気配値を超えた注文はMARKET注文に変更する
          if isBUY(side) then if price > chart.close then (chart.close, true) else (price, false)
          else                if price < chart.close then (chart.close, true) else (price, false)
        Order(childId, side, validPrice, 0, size, expire, positionId, parentId, brotherId, isMarket)
      case STOP(side, triggerPrice, positionId) =>
        check(positionId)
        val (validPrice, isMarket) = // 気配値を超えた注文はMARKET注文に変更する
          if isBUY(side) then if triggerPrice < chart.close then (chart.close, true) else (triggerPrice, false)
          else                if triggerPrice > chart.close then (chart.close, true) else (triggerPrice, false)
        Order(childId, side, 0, validPrice, size, expire, positionId, parentId, brotherId, isMarket)
      case STOP_LIMIT(side, price, triggerPrice, positionId) => 
        check(positionId)
        val validPrice = // 気配値を超えた注文はLIMIT注文に変更する
          if isBUY(side) then if triggerPrice < chart.close then 0 else triggerPrice
          else                if triggerPrice > chart.close then 0 else triggerPrice
        Order(childId, side, price, validPrice, size, expire, positionId, parentId, brotherId)
    
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