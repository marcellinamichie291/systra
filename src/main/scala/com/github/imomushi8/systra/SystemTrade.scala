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
  def backtestStream[Memory:Monoid](brains:       Seq[(String, Brain[BTMarket, Memory])],
                                    firstCapital: Price,
                                    firstChart:   Chart) =
    val initMarket = BTMarket(firstCapital, Nil, Nil, 1, firstChart)
    val initMemory = Monoid[Memory].empty
    val pipes = brains.map { case (name, brain) => (chartStream: Stream[IO, Chart]) => 
      chartStream
        .through(BackTest(brain, initMarket, initMemory))
        .zipWithIndex
        .flatMap { case (reports, index) => Stream( (reports.map{r=>(r.asInstanceOf[PositionReport], index)})* )}
        .through(BackTest.makeSummary(name))
        .map(_.toString)
    }

    (chartStream: Stream[IO, Chart]) => chartStream
      .broadcastThrough(pipes*)

