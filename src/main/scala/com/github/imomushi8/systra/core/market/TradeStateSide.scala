package com.github.imomushi8.systra.core.market

import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.util.Initial

case class TradeSideState(winCount:  Int,
                          loseCount: Int,
                          profit:    Price,
                          loss:      Price,
                          cost:      Price)

object TradeSideState:
  given Initial[TradeSideState] with {
    def empty(): TradeSideState = TradeSideState(0,0,0,0,0) 
  }
                      
  extension (record:TradeSideState)
    def +=(pl: Double, addCost: Double):TradeSideState = if pl > 0 then
      record.copy(winCount = record.winCount+1, profit = record.profit+pl, cost = record.cost+addCost)
    else
      record.copy(loseCount = record.loseCount+1, loss = record.loss+pl, cost = record.cost+addCost)