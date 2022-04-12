package com.github.imomushi8.systra.report

import com.github.imomushi8.systra.util.Initial

case class SideRecord(winCount:  Int,
                      loseCount: Int,
                      profit:    Double,
                      loss:      Double,
                      cost:      Double)

object SideRecord:
  given Initial[SideRecord] with {
    def empty(): SideRecord = SideRecord(0,0,0,0,0) 
  }
                      
  extension (record:SideRecord)
    def +=(pl: Double, addCost: Double):SideRecord = if pl > 0 then
      record.copy(winCount = record.winCount+1, profit = record.profit+pl, cost = record.cost+addCost)
    else
      record.copy(loseCount = record.loseCount+1, loss = record.loss+pl, cost = record.cost+addCost)