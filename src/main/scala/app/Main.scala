package com.github.imomushi8.systra

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.data.ChartStream
import com.github.imomushi8.systra.core.entity.Chart
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.backtest._
import com.github.imomushi8.systra.backtest.BTMarket._

import app.Envs._
import app.demo._

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

object Main extends IOApp:
  import sttp.capabilities.fs2.Fs2Streams
  import sttp.client3._
  import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend
  import sttp.ws.{WebSocket, WebSocketFrame}

  override def run(args: List[String]): IO[ExitCode] = for
    ?   <- IO.println("start")
    res <- bftest()
    ?   <- IO.println("end")
  yield
    ExitCode.Success
    
    
  def bftest() = BitFlyer[M](
    brains, 
    leveragedCapital, 
    BITFLYER_API_KEY, 
    BITFLYER_API_SECRET, 
    BITFLYER_PUBLIC_CHANNELS
    )
    .begin()
    .end()

  def backtest() = 
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