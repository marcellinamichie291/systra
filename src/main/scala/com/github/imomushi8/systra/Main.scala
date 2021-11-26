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
    val seqId = 8
    val charts = List(
      Chart(1,1,1,1,1,LocalDateTime.now),
      Chart(2,2,2,2,2,LocalDateTime.now),
      Chart(3,3,3,3,3,LocalDateTime.now),
      Chart(4,4,4,4,4,LocalDateTime.now),
      Chart(5,5,5,5,5,LocalDateTime.now),
    )

    val orders = List(
      Order("2", BUY, 1, 0, 1, LocalDateTime.now.plusMonths(1), settlePositionId="", isMarket = true),
      Order("3", SELL, 0, 3, 1, LocalDateTime.now.plusMonths(1), settlePositionId="", parentId="2", brotherId="6"),
      Order("4", SELL, 2, 0, 1, LocalDateTime.now.plusMonths(1), settlePositionId="1"),
      Order("5", SELL, 3, 0, 1, LocalDateTime.now.plusMonths(1), settlePositionId=""),
      Order("6", BUY, 2, 0, 1, LocalDateTime.now.plusMonths(1), settlePositionId="", parentId="2", brotherId="3"),
      Order("7", BUY, 1, 3, 1, LocalDateTime.now.plusMonths(1), settlePositionId="")
    )

    val positions = List(
      Position(LocalDateTime.now, "1", BUY, 1, 1)
    )

    val initMarket = BTMarket(1000, orders, positions, seqId, charts.head, 8)
    //val initMarket = BTMarket(1000, Nil, Nil, 1, charts.head, 0)
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
        println("REPORT\n" + reports)
        println("------------------------------------------------------------------")
        println("DISTINCT REPORT\n" + reports.distinct)
        
      }.as(ExitCode.Success)

