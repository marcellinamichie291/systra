package com.github.imomushi8.systra

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.data.ChartStream
import com.github.imomushi8.systra.core.entity.Chart
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.backtest._
import com.github.imomushi8.systra.backtest.BTMarket._
import com.github.imomushi8.systra.backtest.BackTest._
import app.brain._

import mgo.evolution._

import java.time.LocalDateTime
import concurrent.duration.DurationInt

import cats.implicits._
import cats.effect._
import fs2._
import fs2.concurrent._
import fs2.io.file.{Files, Path}

import com.github.gekomad.ittocsv.parser.io.FromFile.csvFromFileStream
import com.github.gekomad.ittocsv.core.Types.implicits._
import com.github.gekomad.ittocsv.core.ToCsv
import com.github.gekomad.ittocsv.core.ToCsv._
import com.github.gekomad.ittocsv.core.FromCsv.Decoder
import com.github.gekomad.ittocsv.parser.{IttoCSVFormat, StringToCsvField}

/*
 * ポジションのすべてを閉じるのではなく、sizeの差分だけ決済する処理を行うシストレの新しいフレームワークが欲しい
 * FX等の信用取引の場合は注文ID等で管理すると思われるため、現在作成中の構成で行う。
 */

object Main extends IOApp:

  given IttoCSVFormat = IttoCSVFormat.default
  given FieldEncoder[SummarySubReport] = customFieldEncoder[SummarySubReport](_.toString)

  type M = ControlChartBrain.Memory

  val brainName="ControlChartBrain"
  val brains = { for
      t <- 24 to 96 by 24
      maxBias <- 10 to 55 by 15
      maxUpDown <- 10 to 55 by 15
      maxScat <- 30 to 50 by 20
    yield (
      s"$brainName($t $maxBias $maxUpDown $maxScat)", 
      ControlChartBrain[BTMarket](t, maxBias, maxUpDown, maxScat))
    //List((s"$brainName(36 8 8 15)", ControlChartBrain[BTMarket](36, 8, 8, 15)))
    //for t <- 36 until 108 by 6 yield (s"$brainName($t 8 8 15)", ControlChartBrain[BTMarket](t, 8, 8, 15))
  }

  val firstCapital = 1_000_000.0 // 100万円
  val leverage = 25.0 // レバレッジ
  val leveragedCapital = firstCapital*leverage
  
  val readCsvPath = "csv_chart/USDJPY_2015_2021/USDJPY_2016_all.csv"
  val writeCsvPath = s"reports/${brainName}_2016_5.csv"

  override def run(args: List[String]): IO[ExitCode] = BackTest[M](brains, leveragedCapital, readCsvPath)
    .begin[OHLCV] { csv =>
      val datetime = LocalDateTime.parse(s"${csv.dateStr} ${csv.timeStr}", csvDatetimeFormatter)
        Chart(csv.open, csv.high, csv.low, csv.close, csv.volume, datetime)
    }
    .downSampling()
    .end(writeCsvPath)
    .compile
    .drain
    .handleError { t => t.printStackTrace }
    .flatMap{_=> IO.println(s"Done Write CSV: $writeCsvPath") }
    .as(ExitCode.Success)