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
import com.github.gekomad.ittocsv.core.ToCsv._
import com.github.gekomad.ittocsv.parser.IttoCSVFormat

object SystemTrade:
  given IttoCSVFormat = IttoCSVFormat.default

  def writePositionReportToCsv(writeCsvPath: String):Pipe[IO, PositionReport, INothing] = 
    _.map(toCsv(_))
    .intersperse(System.lineSeparator)
    .through(text.utf8.encode)
    .through(Files[IO].writeAll(Path(writeCsvPath)))

  def backtestStreamReportToCsv[Memory:Monoid](writeCsvPath: String,
                                        chartStream:  Stream[IO, Chart],
                                        firstCapital: Price,
                                        dummyChart:   Chart,
                                        brain:        Brain[BTMarket, Memory]): Stream[IO, INothing] =
    val tradeFunc = BackTest.trade(brain)
    val initMarket = BTMarket(firstCapital, Nil, Nil, 1, dummyChart, 0)
    val initMemory = Monoid[Memory].empty
    chartStream
      .through(BackTest(tradeFunc, initMarket, initMemory))
      .flatMap { reports => Stream((reports.map(_.asInstanceOf[PositionReport]))*)}
      .through(writePositionReportToCsv(writeCsvPath))