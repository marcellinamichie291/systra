package app.model.service

import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.core.action._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.virtual.VirtualMarket._

import app.demo._
import app.bitflyer._
import app.backtest._
import app.backtest.report._

import cats.data.Kleisli
import cats.syntax.all._
import cats.implicits._
import cats.effect._

import com.github.gekomad.ittocsv.core.Types.implicits._
import com.github.gekomad.ittocsv.core.ToCsv._
import com.github.gekomad.ittocsv.parser.IttoCSVFormat
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime

import com.typesafe.scalalogging.LazyLogging
import fs2.concurrent.SignallingRef
import app.model.AppStatus

class BackTestService[Memory: Initial](brains: Seq[(String, Brain[VirtualMarket, Memory])], 
                                       capital: Price,
                                       readCsvPath: String,
                                       writeCsvPath: String)
                                      (using clock: Clock[IO]) extends Service with LazyLogging {
  given IttoCSVFormat = IttoCSVFormat.default
  given FieldEncoder[SummarySubReport] = customFieldEncoder[SummarySubReport](_.toString)
  val csvDatetimeFormatter = DateTimeFormatter.ofPattern("yyyy.MM.dd HH:mm")                                 

  override def getApp: Kleisli[IO, SignallingRef[IO, AppStatus[Service]], Unit] = Kleisli { _ =>    
    val application = BackTest(brains, capital)
      .begin[app.backtest.OHLCV](readCsvPath) { csv =>
        val datetime = LocalDateTime.parse(s"${csv.dateStr} ${csv.timeStr}", csvDatetimeFormatter)
          Chart(csv.open, csv.high, csv.low, csv.close, csv.volume, datetime)
      }
      .downSampling()
      .end(writeCsvPath)
      .handleError { t => t.printStackTrace }

    for
      start  <- clock.monotonic
      res    <- application
      finish <- clock.monotonic
    yield
      logger.info(s"Backtest elapsed time: ${(finish - start).toSeconds} s")
  }
}
