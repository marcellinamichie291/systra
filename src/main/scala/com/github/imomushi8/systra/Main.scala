package com.github.imomushi8.systra

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.backtest._
import com.github.imomushi8.systra.backtest.BTMarket._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report.{Report, PositionReport}
import com.github.imomushi8.systra.brain.MockBrain

import java.time.LocalDateTime
import concurrent.duration.DurationInt

import fs2._
import cats.data.State
import cats.kernel.Monoid
import cats.implicits._
import cats.effect._
import cats.effect.unsafe.implicits.global

object Main extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    val seqId = 7
    val charts = List(
      Chart(1,1,1,1,1,LocalDateTime.now),
      Chart(2,2,2,2,2,LocalDateTime.now),
      Chart(3,3,3,3,3,LocalDateTime.now),
      Chart(4,4,4,4,4,LocalDateTime.now),
      Chart(5,5,5,5,5,LocalDateTime.now),
    )

    val orders = List(
      Order("2", BUY, 3, 0, 1, LocalDateTime.now, settlePositionId="", isMarket = true),
      Order("3", BUY, 0, 3, 1, LocalDateTime.now, settlePositionId=""),
      Order("4", BUY, 2, 0, 1, LocalDateTime.now, settlePositionId="", isMarket = true),
      Order("5", SELL, 2, 0, 1, LocalDateTime.now, settlePositionId="", parentId="2"),
      Order("6", SELL, 3, 0, 1, LocalDateTime.now, settlePositionId="1")
    )

    val positions = List(
      Position(LocalDateTime.now, "1", BUY, 10, 1)
    )

    //val initMarket = BTMarket(1000, orders, positions, seqId, charts.head, 7)
    val initMarket = BTMarket(1000, Nil, Nil, 1, charts.head, 0)
    val initMemory = Monoid[MockBrain.Memory].empty
    val brain = MockBrain[BTMarket](1)
    val initState = (initMarket, initMemory)
    val init = State.set(initState) *> State.pure(Vector[Report]())
    val backtest = BackTest[MockBrain.Memory](brain)

    Stream[IO, Chart](charts*)
      .scan((initState, Vector[Report]())) { (current, chart) => backtest(chart).run(current._1).value } 
      .map(_._2)
      .compile
      .foldMonoid
      .handleError { t => 
        t.printStackTrace
        Vector()
      }
      .map {reports => 
        println("------------------------------------------------------------------")
        println("REPORT " + reports)
        println("------------------------------------------------------------------")
        println("DISTINCT REPORT " + reports.distinct)
        
      }.as(ExitCode.Success)

