package com.github.imomushi8.systra

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.data.ChartStream
import com.github.imomushi8.systra.core.entity.Chart
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.virtual.VirtualMarket._

import app.Envs._
import app.bitflyer._
import app.backtest._

import mgo.evolution._

import cats.implicits._
import cats.effect._
import fs2._


object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] = for
    ?   <- IO.println("start")
    res <- bfDemo()
    ?   <- IO.println("end")
  yield
    ExitCode.Success
    
    
  def bfDemo() = BitFlyerDemo[M](
    brains, 
    leveragedCapital, 
    BITFLYER_API_KEY, 
    BITFLYER_API_SECRET, 
    BITFLYER_PUBLIC_CHANNELS.head)
    .begin()
    .end()

  def backtest() = 
    import com.github.gekomad.ittocsv.core.Types.implicits._
    import com.github.gekomad.ittocsv.core.ToCsv._
    import com.github.gekomad.ittocsv.parser.IttoCSVFormat
    import java.time.LocalDateTime

    given IttoCSVFormat = IttoCSVFormat.default
    given FieldEncoder[SummarySubReport] = customFieldEncoder[SummarySubReport](_.toString)
    
    BackTest[M](brains, leveragedCapital)
      .begin[OHLCV](readCsvPath) { csv =>
        val datetime = LocalDateTime.parse(s"${csv.dateStr} ${csv.timeStr}", csvDatetimeFormatter)
          Chart(csv.open, csv.high, csv.low, csv.close, csv.volume, datetime)
      }
      .downSampling()
      .end(writeCsvPath)
      .handleError { t => t.printStackTrace }
      .as(ExitCode.Success)