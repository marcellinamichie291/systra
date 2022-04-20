package com.github.imomushi8.systra.demo

import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.virtual.VirtualMarket
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.market._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.util.Initial

import cats.effect.IO
import fs2._

trait Demo extends Tradable[VirtualMarket]:
  def summerize[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])],
                                 firstCapital: Price,
                                 firstChart:   Chart): Seq[Pipe[IO, Chart, SummaryReport]] =
    val initSub = SummarySubReport(0,0,0,0,0)
    val initSummary = SummaryReport("", 0, 0, 0, OK, initSub, initSub, initSub, 0, 0, 0)
    given initMarket: Initial[VirtualMarket] = VirtualMarket.initial(firstCapital, firstChart)
    
    brains.map { case (name, brain) => (chartStream: Stream[IO, Chart]) => 
      chartStream
        .through(trade(brain))
        .fold(initSummary) { 
          case (current, (_, _, TradeState(sampleSize, flag, buy, sell, capital, maxCapital, consWins, consLoses, _))) =>
            val buySub  = SummarySubReport(buy.winCount, buy.loseCount, buy.profit, buy.loss, buy.cost)
            val sellSub = SummarySubReport(sell.winCount, sell.loseCount, sell.profit, sell.loss, sell.cost)
            val maximalDrawDown = current.maximalDrawDown max (maxCapital - capital)
            val consecutiveWinCount  = current.consecutiveWinCount max consWins
            val consecutiveLoseCount = current.consecutiveLoseCount max consLoses
            SummaryReport(name, sampleSize, firstCapital, capital, flag, buySub, sellSub, buySub+sellSub, maximalDrawDown, consecutiveWinCount, consecutiveLoseCount)
        }
    }