package com.github.imomushi8.systra.util

import com.github.imomushi8.systra.entity._

/** BUYならtrue, SELLならfalse */
def isBUY(side: Side): Boolean = side > 0

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
