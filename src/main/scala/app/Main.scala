package com.github.imomushi8.systra

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity.Chart
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.virtual.VirtualMarket._

import app.Envs._
import app.demo._
import app.bitflyer._
import app.backtest._

import cats.implicits._
import cats.effect._

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] = for
    ?   <- IO.println("start")
    res <- bfDemo().handleError { t => println("error occured") }
    ?   <- IO.println("end")
  yield
    ExitCode.Success

  def test() = IO.println {
    import io.circe.Decoder
    import io.circe.generic.semiauto._
    import io.circe.parser._
    decode[JsonRpcRes[BFRes.Execution]](
    """{
    | "jsonrpc":"2.0",
    | "method":"test",
    | "params":{
    |   "channel":"TEST",
    |   "message":[{
    |       "id":414, 
    |       "side:"BUY",
    |       "price:114514.9,
    |       "size:431.2,
    |       "exec_date:"exec String",
    |       "buy_child_order_acceptance_id:"buy String"
    |       "sell_child_order_acceptance_id:"sell String"
    |   }]
    | }
    |}""").stripMargin
  }
  
  def bfDemo() = for
    apiKey    <- BITFLYER_API_KEY.load[IO]
    apiSecret <- BITFLYER_API_SECRET.load[IO]
    channel   <- BITFLYER_PUBLIC_CHANNEL.load[IO]
    ?   <- IO.println(BFUtils.authText(apiKey.value, apiSecret.value))
    ?   <- test()
    ws        <- IO{ DemoBitFlyerWS(brains, leveragedCapital, apiKey.value, apiSecret.value, channel) }
    ?         <- Demo.begin(ws).end()
  yield ()

/*
  def backtest() = 
    import com.github.gekomad.ittocsv.core.Types.implicits._
    import com.github.gekomad.ittocsv.core.ToCsv._
    import com.github.gekomad.ittocsv.parser.IttoCSVFormat
    import java.time.format.DateTimeFormatter
    import java.time.LocalDateTime
    
    val csvDatetimeFormatter = DateTimeFormatter.ofPattern("yyyy.MM.dd HH:mm")

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
  */