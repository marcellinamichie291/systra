package app

import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity.Chart
import com.github.imomushi8.systra.virtual.VirtualMarket._

import app.Envs._
import app.demo._
import app.bitflyer._
import app.backtest._

import cats.syntax.all._
import cats.implicits._
import cats.effect._

import fs2._
import sttp.ws.WebSocketFrame

import com.typesafe.scalalogging.LazyLogging

import java.time.temporal.TemporalAmount
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.DurationInt

object Main extends LazyLogging, IOApp:
  override def run(args: List[String]): IO[ExitCode] = for
    ?   <- IO.println("start")
    res <- bfDemo(java.time.Duration.ofMinutes(1L))
    ?   <- IO.println("end")
  yield
    ExitCode.Success
  
  def bfDemo(period: TemporalAmount) = for
    apiKey    <- BITFLYER_API_KEY.load[IO]
    apiSecret <- BITFLYER_API_SECRET.load[IO]
    channel   <- BITFLYER_PUBLIC_CHANNEL.load[IO]
    signal    <- SignallingRef[IO, Boolean](false)
    ws        <- IO { new BitFlyerWS(apiKey.value, apiSecret.value, channel, period) {
      override def executeTrade(chartStream: Stream[IO, Chart]): Stream[IO, WebSocketFrame] = chartStream
        .head
        .flatMap { head => chartStream
          .through(Demo(brains, firstCapital, head))
          .drain
        } 
      }
    }
    ? <- signal.set(true).timeout(10.seconds)
    ? <- Demo.begin(ws, signal).end()
  yield ()

  def backtest()(using clock: Clock[IO]) = 
    import app.backtest.report._
    import com.github.gekomad.ittocsv.core.Types.implicits._
    import com.github.gekomad.ittocsv.core.ToCsv._
    import com.github.gekomad.ittocsv.parser.IttoCSVFormat
    import java.time.format.DateTimeFormatter
    import java.time.LocalDateTime
    
    val csvDatetimeFormatter = DateTimeFormatter.ofPattern("yyyy.MM.dd HH:mm")

    given IttoCSVFormat = IttoCSVFormat.default
    given FieldEncoder[SummarySubReport] = customFieldEncoder[SummarySubReport](_.toString)
    
    val application = BackTest[M](brains, leveragedCapital)
      .begin[OHLCV](readCsvPath) { csv =>
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
      msg    <- IO(s"Backtest elapsed time: ${(finish - start).toSeconds} s")
    yield
      logger.info(msg)
