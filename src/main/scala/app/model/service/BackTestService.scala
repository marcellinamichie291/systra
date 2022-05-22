package app.model.service

import com.github.imomushi8.systra.chart._
import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.market.{Tradable, OK, TradeState}

import app.demo._
import app.bitflyer._
import app.backtest._
import app.backtest.report._

import cats.data.Kleisli
import cats.syntax.all._
import cats.implicits._
import cats.effect._

import fs2._
import fs2.io.file.{Files, Path}


import com.github.gekomad.ittocsv.core.Types.implicits._
import com.github.gekomad.ittocsv.core.ToCsv._
import com.github.gekomad.ittocsv.parser.IttoCSVFormat
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime

import com.typesafe.scalalogging.LazyLogging
import fs2.concurrent.SignallingRef
import app.model.AppStatus

import com.github.gekomad.ittocsv.core.FromCsv.Decoder
import com.github.gekomad.ittocsv.parser.IttoCSVFormat
import deriving.Mirror.ProductOf
import java.time.temporal.TemporalAmount

class BackTestService[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])], 
                                       firstCapital: Price,
                                       readCsvPath:  String,
                                       writeCsvPath: String)
                                      (using clock:  Clock[IO]) extends Service with LazyLogging {
  given IttoCSVFormat = IttoCSVFormat.default
  given FieldEncoder[SummarySubReport] = customFieldEncoder[SummarySubReport](_.toString)
  val csvDatetimeFormatter = DateTimeFormatter.ofPattern("yyyy.MM.dd HH:mm")                                 

  override def getApp: Kleisli[IO, SignallingRef[IO, AppStatus[Service]], Unit] = Kleisli { status =>
    for
      start  <- clock.monotonic
      res    <- application(status)
      finish <- clock.monotonic
    yield
      logger.info(s"Backtest elapsed time: ${(finish - start).toSeconds} s")
  }

  private def application(status: SignallingRef[IO, AppStatus[Service]]) = 
    begin[app.backtest.OHLCV](readCsvPath) { csv =>
      val datetime = LocalDateTime.parse(s"${csv.dateStr} ${csv.timeStr}", csvDatetimeFormatter)
        Chart(csv.open, csv.high, csv.low, csv.close, csv.volume, datetime)
    }
    .downSampling(null)
    .end(writeCsvPath)
    .handleError { t => t.printStackTrace }


  /** 開始メソッド */
  def begin[Csv](csvPath:    String)
                (csvToChart: Csv => Chart)
                (using p:    ProductOf[Csv],
                       d:    Decoder[List[String], p.MirroredElemTypes],
                       i:    IttoCSVFormat): Ops = Ops(CsvStream(csvPath)(csvToChart))

  class Ops(stream: Stream[IO, Chart]):
    /** ダウンサンプリング */
    def downSampling(period: TemporalAmount): Ops = new Ops(stream.through(CsvStream.downSampling(period)))

    /** 終了メソッド */
    def end(writeCsvPath: String): IO[Unit] = Stream
      .emit[IO, String](SummaryReport.toList.mkString(",")) // headerを作成
      .append(stream
        .through(BackTest(brains, firstCapital))
        .map(_.toString)
      )
      .intersperse(System.lineSeparator)
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path(writeCsvPath)))
      .compile
      .drain
      .>> { IO.println(s"Done Write CSV: $writeCsvPath") } 
}
