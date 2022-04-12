package com.github.imomushi8.systra.report

import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.behavior.Flag
import com.github.imomushi8.systra.util.Price
import com.github.imomushi8.systra.behavior.OK
import com.github.imomushi8.systra.util.Initial

case class TradeState(sampleSize:       Int,
                      flag:             Flag,
                      buyRecord:        SideRecord,
                      sellRecord:       SideRecord,
                      capital:          Price,
                      maxCapital:       Price, 
                      consecutiveWins:  Int,
                      consecutiveLoses: Int,
                      transactions:     Vector[PositionReport])

object TradeState:
  def initial(capital: Price): Initial[TradeState] = new Initial[TradeState] {
    def empty(): TradeState = 
      val initSide = Initial[SideRecord]()
      TradeState(0, OK, initSide, initSide, capital, capital, 0, 0, Vector())
  }