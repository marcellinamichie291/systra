package com.github.imomushi8.systra

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.entity._
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.backtest.{BackTest, BTMarket}

import cats.effect._
import cats.kernel.Monoid

import fs2._
import fs2.io.file.{Files, Path}
import com.github.gekomad.ittocsv.core.Types.implicits._
import com.github.gekomad.ittocsv.core.ToCsv
import com.github.gekomad.ittocsv.core.ToCsv._
import com.github.gekomad.ittocsv.core.ToCsv.given
import com.github.gekomad.ittocsv.core.FromCsv.Decoder
import com.github.gekomad.ittocsv.parser.{IttoCSVFormat, StringToCsvField}

object SystemTrade:
  given IttoCSVFormat = IttoCSVFormat.default
  given FieldEncoder[SummarySubReport] = customFieldEncoder[SummarySubReport](_.toString)
  val initSub = SummarySubReport(0,0,0,0,0)
  val initSummary = SummaryReport("", 0, initSub, initSub, initSub, 0, 0, 0)

  def makeSummary(writeCsvPath: String,
                  traderName:   String,
                  sampleSize:   Int): Pipe[IO, PositionReport, SummaryReport] = _
    .map{ record =>
      val pl = record.side*(record.closePrice - record.openPrice)*record.size - record.cost
      (record.side, pl, record.cost) /* side, pl, cost */
    }
    .fold((initSummary, 0.0, 0.0, 0, 0)) { 
      case ((currentSummary, currentCapital, currentMax, currentSuccWin, currentSuccLose),
            (side, pl, cost)) =>
        val nextCapital = currentCapital + pl
        val nextMax = currentMax max nextCapital
        val (nextSuccWin, nextSuccLose) = 
          if pl > 0 then (currentSuccWin+1, 0)
          else           (0, currentSuccLose+1)
        val nextSummary = currentSummary ++ (traderName, sampleSize, currentCapital, currentMax, currentSuccWin, currentSuccLose, side, pl, cost)
        (nextSummary, nextCapital, nextMax, nextSuccWin, nextSuccLose)
    }
    .map(_._1)

  def backtestStreamReportToCsv[Memory:Monoid](traderName:   String,
                                               sampleSize:   Int,
                                               writeCsvPath: String,
                                               firstCapital: Price,
                                               dummyChart:   Chart,
                                               brain:        Brain[BTMarket, Memory])
                                              (chartStream:  Stream[IO, Chart]) =
    val tradeFunc = BackTest.trade(brain)
    val initMarket = BTMarket(firstCapital, Nil, Nil, 1, dummyChart, 0)
    val initMemory = Monoid[Memory].empty
    chartStream
      .through(BackTest(tradeFunc, initMarket, initMemory))
      .flatMap { reports => Stream((reports.map(_.asInstanceOf[PositionReport]))*)}
      .through(makeSummary(writeCsvPath, traderName, sampleSize))
      .map(_.toString)
      .intersperse(System.lineSeparator)
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path(writeCsvPath)))