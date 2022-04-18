package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.data.ChartStream
import com.github.imomushi8.systra.core.market.{Tradable, OK, TradeState}

import cats.implicits._
import cats.effect._
import fs2._
import fs2.io.file.{Files, Path}

import com.github.gekomad.ittocsv.core.FromCsv.Decoder
import com.github.gekomad.ittocsv.parser.IttoCSVFormat
import deriving.Mirror.ProductOf

/** Tradableのバックテスト用インスタンス */
class BackTest[Memory: Initial](brains:       Seq[(String, Brain[BTMarket, Memory])],
                                firstCapital: Price) extends Tradable[BTMarket]:
  /** BackTestの開始メソッド */
  def begin[Csv](readCsvPath:  String)
                  (csvToChart:   Csv => Chart)
                  (using p:      ProductOf[Csv],
                         d:      Decoder[List[String], p.MirroredElemTypes],
                         i:      IttoCSVFormat): BackTestOps[Memory] = 
    BackTestOps[Memory](BackTestStream[Csv](readCsvPath, csvToChart).begin())
  
  def summerize[Memory:Initial](firstChart: Chart) =
    val initSub = SummarySubReport(0,0,0,0,0)
    val initSummary = SummaryReport("", 0, 0, 0, OK, initSub, initSub, initSub, 0, 0, 0)
    given initMarket: Initial[BTMarket] = BTMarket.initial(firstCapital, firstChart)
    brains.map { case (name, brain) => (chartStream: Stream[IO, Chart]) => 
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


  class BackTestOps[Memory: Initial](stream: Stream[IO, Chart]):
    /** ダウンサンプリング */
    def downSampling(): BackTestOps[Memory] = new BackTestOps(stream.head >>= { head => stream.through(ChartStream.downSampling(head)) }
    )

    /** BackTestの終了メソッド */
    def end(writeCsvPath: String) = 
      val summaryStream = stream.head >>= { head => stream
        .broadcastThrough(summerize[Memory](head)*)
        .map(_.toString)
      }
      
      Stream
      .emit[IO, String](SummaryReport.toList.mkString(",")) // headerを作成
      .append(summaryStream)
      .intersperse(System.lineSeparator)
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path(writeCsvPath)))
      .compile
      .drain
      .>> { IO.println(s"Done Write CSV: $writeCsvPath") }