package app.demo

import com.github.imomushi8.systra.chart._
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.virtual.VirtualMarket
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.market._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.util.Initial

import cats.effect.IO
import fs2._

object Demo extends Tradable[VirtualMarket]:
  def summerize[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])],
                                 firstCapital: Price,
                                 firstChart:   Chart): Seq[Pipe[IO, Chart, SummaryReport]] = 
    given initMarket: Initial[VirtualMarket] = VirtualMarket.initial(firstCapital, firstChart)
    brains.map { case (name, brain) => (chartStream: Stream[IO, Chart]) => 
      chartStream
        .through(trade(brain))
        .fold(Initial[SummaryReport]()) { 
          case (current, (_, _, TradeState(sampleSize, flag, buy, sell, capital, maxCapital, consWins, consLoses, _))) =>
            val buySub  = SummarySubReport(buy.winCount, buy.loseCount, buy.profit, buy.loss, buy.cost)
            val sellSub = SummarySubReport(sell.winCount, sell.loseCount, sell.profit, sell.loss, sell.cost)
            val maximalDrawDown = current.maximalDrawDown max (maxCapital - capital)
            val consecutiveWinCount  = current.consecutiveWinCount max consWins
            val consecutiveLoseCount = current.consecutiveLoseCount max consLoses
            SummaryReport(name, sampleSize, firstCapital, capital, flag, buySub, sellSub, buySub+sellSub, maximalDrawDown, consecutiveWinCount, consecutiveLoseCount)
        }
    }

  /** 開始メソッド */
  def begin(ws: WebSocketStream): Ops = new Ops(ws)
 
  class Ops(ws: WebSocketStream):
    /** Demoの終了メソッド */
    def end() = ws() >> { IO.println("Done DemoTrade") }