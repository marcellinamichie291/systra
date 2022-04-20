package app.backtest

import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.data._
import com.github.imomushi8.systra.core.market.{Tradable, OK, TradeState}

import cats.implicits._
import cats.effect._
import fs2._
import fs2.io.file.{Files, Path}

import com.github.gekomad.ittocsv.core.FromCsv.Decoder
import com.github.gekomad.ittocsv.parser.IttoCSVFormat
import deriving.Mirror.ProductOf

/** バックテスト用クラス */
class BackTest[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])],
                                firstCapital: Price) extends Tradable[VirtualMarket], CsvStream:
  override def summarize(chartStream: Stream[IO, Chart]): Stream[IO, SummaryReport] = chartStream.head >>= { head => 
    val initSub = SummarySubReport(0,0,0,0,0)
    val initSummary = SummaryReport("", 0, 0, 0, OK, initSub, initSub, initSub, 0, 0, 0)
    given initMarket: Initial[VirtualMarket] = VirtualMarket.initial(firstCapital, head)

    val pipes = brains.map { case (name, brain) => (chartStream: Stream[IO, Chart]) => 
      chartStream
        .through(trade(brain))
        .fold(initSummary) { 
          case (current, (_, _, TradeState(sampleSize, flag, buy, sell, capital, maxCapital, consWins, consLoses, _))) =>
            val buySub = SummarySubReport(buy.winCount, buy.loseCount, buy.profit, buy.loss, buy.cost)
            val sellSub = SummarySubReport(sell.winCount, sell.loseCount, sell.profit, sell.loss, sell.cost)
            val maximalDrawDown = current.maximalDrawDown max (maxCapital - capital)
            val consecutiveWinCount  = current.consecutiveWinCount max consWins
            val consecutiveLoseCount = current.consecutiveLoseCount max consLoses
            SummaryReport(name, sampleSize, firstCapital, capital, flag, buySub, sellSub, buySub+sellSub, maximalDrawDown, consecutiveWinCount, consecutiveLoseCount)
        }
    }

    chartStream
      .broadcastThrough(pipes*)
  }
    
    