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

import com.github.gekomad.ittocsv.core.FromCsv.Decoder
import com.github.gekomad.ittocsv.parser.IttoCSVFormat
import deriving.Mirror.ProductOf

class BackTest[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])],
                                firstCapital: Price) extends Tradable[VirtualMarket]:

  def summarize(chartStream: Stream[IO, Chart]): Stream[IO, SummaryReport] = chartStream.head >>= { head => 
    given initMarket: Initial[VirtualMarket] = VirtualMarket.initial(firstCapital, head)
    val pipes = brains.map { case (name, brain) => (chartStream: Stream[IO, Chart]) => 
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
    chartStream.broadcastThrough(pipes*)
  }

  /** 開始メソッド */
  def begin[Csv](csvPath:    String)
                (csvToChart: Csv => Chart)
                (using p:    ProductOf[Csv],
                       d:    Decoder[List[String], p.MirroredElemTypes],
                       i:    IttoCSVFormat): Ops = Ops(CsvStream(csvPath)(csvToChart))

  class Ops(stream: Stream[IO, Chart]):
    /** ダウンサンプリング 
     * TODO: あとでちゃんと実装
    */
    def downSampling(): Ops = new Ops(stream.through(CsvStream.downSampling(null)))

    /** 終了メソッド */
    def end(writeCsvPath: String): IO[Unit] = Stream
      .emit[IO, String](SummaryReport.toList.mkString(",")) // headerを作成
      .append(stream
        .through(summarize)
        .map(_.toString)
      )
      .intersperse(System.lineSeparator)
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path(writeCsvPath)))
      .compile
      .drain
      .>> { IO.println(s"Done Write CSV: $writeCsvPath") } 
