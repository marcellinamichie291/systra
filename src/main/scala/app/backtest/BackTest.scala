package app.backtest

import app.backtest.report._

import com.github.imomushi8.systra.chart._
import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.market.{Tradable, OK, TradeState}

import cats.implicits._
import cats.effect._
import fs2._
import fs2.io.file.{Files, Path}

object BackTest extends Tradable[VirtualMarket]:
  def apply[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])],
                             firstCapital: Price): Pipe[IO, Chart, SummaryReport] = chartStream => 
    chartStream
      .head >>= { head => 
        given initMarket: Initial[VirtualMarket] = VirtualMarket.initial(firstCapital, head)
        chartStream.broadcastThrough(pipes(brains, firstCapital)*)
      }

  private def pipes[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])],
                                     firstCapital: Price)
                                    (using initMarket: Initial[VirtualMarket]) = brains
    .map { case (name, brain) => (chartStream: Stream[IO, Chart]) => 
      chartStream
        .through(trade(brain))
        .fold(Initial[SummaryReport]()) { 
          case (current, (_, _, TradeState(sampleSize, flag, buy, sell, capital, maxCapital, consWins, consLoses, _))) =>
            val buySub = SummarySubReport(buy.winCount, buy.loseCount, buy.profit, buy.loss, buy.cost)
            val sellSub = SummarySubReport(sell.winCount, sell.loseCount, sell.profit, sell.loss, sell.cost)
            val maximalDrawDown = current.maximalDrawDown max (maxCapital - capital)
            val consecutiveWinCount  = current.consecutiveWinCount max consWins
            val consecutiveLoseCount = current.consecutiveLoseCount max consLoses
            SummaryReport(name, sampleSize, firstCapital, capital, flag, buySub, sellSub, buySub+sellSub, maximalDrawDown, consecutiveWinCount, consecutiveLoseCount)
        }
    }
