package com.github.imomushi8.systra.core.market

import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.util.Initial

case class TradeState(sampleSize:       Int,
                      flag:             Flag,
                      buyRecord:        TradeSideState,
                      sellRecord:       TradeSideState,
                      capital:          Price,
                      maxCapital:       Price, 
                      consecutiveWins:  Int,
                      consecutiveLoses: Int,
                      transactions:     Vector[PositionTransaction])

object TradeState:
  def initial(capital: Price): Initial[TradeState] = new Initial[TradeState] {
    def empty(): TradeState = 
      val initSide = Initial[TradeSideState]()
      TradeState(0, OK, initSide, initSide, capital, capital, 0, 0, Vector())
  }