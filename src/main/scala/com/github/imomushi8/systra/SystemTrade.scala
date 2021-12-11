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

object SystemTrade:
  def backtestStreamReportToCsv[Memory:Monoid](brain:        Brain[BTMarket, Memory],
                                               traderName:   String,firstCapital: Price,
                                               firstChart:   IO[Chart],
                                               writeCsvPath: String) = firstChart.map { head =>
    val initMarket = BTMarket(firstCapital, Nil, Nil, 1, head, 0)
    val initMemory = Monoid[Memory].empty
    
    (chartStream:  Stream[IO, Chart]) => 
      chartStream
        .through(BackTest(brain, initMarket, initMemory))
        .zipWithIndex
        .flatMap { case (reports, index) => Stream((reports.map{r=>(r.asInstanceOf[PositionReport], index) })* )}
        .through(BackTest.makeSummary(writeCsvPath, traderName))
        .map(_.toString)
        .intersperse(System.lineSeparator)
        .through(text.utf8.encode)
        .through(Files[IO].writeAll(Path(writeCsvPath)))
  }