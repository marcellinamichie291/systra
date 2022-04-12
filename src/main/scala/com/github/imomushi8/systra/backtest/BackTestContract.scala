package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.core.util.{isSTOP_LIMIT, isContracted}
import com.github.imomushi8.systra.core.entity._

import cats.implicits._

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