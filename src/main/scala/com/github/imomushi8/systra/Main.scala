package com.github.imomushi8.systra

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.entity._
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.brain._

import com.github.imomushi8.systra.backtest._
import com.github.imomushi8.systra.backtest.BTMarket._
import com.github.imomushi8.systra.SystemTrade.given_IttoCSVFormat

import java.time.LocalDateTime
import concurrent.duration.DurationInt

import cats.implicits._
import cats.effect._
import fs2._
import fs2.concurrent._

import com.github.gekomad.ittocsv.parser.io.FromFile.csvFromFileStream
import com.github.gekomad.ittocsv.core.Types.implicits._
import com.github.gekomad.ittocsv.core.ToCsv
import com.github.gekomad.ittocsv.core.ToCsv._
import com.github.gekomad.ittocsv.core.FromCsv.Decoder
import com.github.gekomad.ittocsv.parser.{IttoCSVFormat, StringToCsvField}
import cats.kernel.Monoid

/*
 * ポジションのすべてを閉じるのではなく、sizeの差分だけ決済する処理を行うシストレの新しいフレームワークが欲しい
 * FX等の信用取引の場合は注文ID等で管理すると思われるため、現在作成中の構成で行う。
 */ 

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val readCsvPath = "csv_chart/USDJPY_2015_2021/USDJPY_2021_02.csv"
    val writeCsvPath = "reports/test.csv"
    val firstCapital = 10000000
    val dummyChart = Chart(0,0,0,0,0,LocalDateTime.now)
    val brain = MockBrain[BTMarket](1)

    /* Chartを流すStream */
    val chartStream = csvFromFileStream[OHLCV](readCsvPath, skipHeader = false)
      .scan(dummyChart) { (prev, csvEither) => csvEither match 
        case Left(_) => prev
        case Right(csv) => 
          val datetime = LocalDateTime.parse(s"${csv.dateStr} ${csv.timeStr}", csvDatetimeFormatter)
          Chart(csv.open, csv.high, csv.low, csv.close, csv.volume, datetime)
      }
      
    /* ここでstreamを取得する */
    val backtestPipe = SystemTrade.backtestStreamReportToCsv[MockBrain.Memory](
      "MockBrain",
      0,
      writeCsvPath,
      firstCapital,
      dummyChart,
      brain)
      
    /* 実行 */
    chartStream
      .through(backtestPipe)
      .compile
      .drain
      .flatMap{_=> IO.println("Done Write CSV") }
      .handleError { t => t.printStackTrace }
      .as(ExitCode.Success)