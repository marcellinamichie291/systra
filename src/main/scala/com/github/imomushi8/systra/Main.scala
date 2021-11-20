package com.github.imomushi8.systra

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.backtest._
import com.github.imomushi8.systra.backtest.BTMarket._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report.{Report, PositionReport}
import com.github.imomushi8.systra.brain.MockBrain

import java.time.LocalDateTime
import fs2._
import cats.implicits._
import cats.kernel.Monoid
import org.atnos.eff.syntax.all._
import org.atnos.eff._
import java.time.LocalDate
import cats.effect.IO
import cats.effect.unsafe.implicits.global

object Main extends App:
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

  val market = BTMarket(100000, orders, positions, seqId, charts.head, 7)
  val memory = Monoid[MockBrain.Memory].empty
  val brain = MockBrain[BTMarket](2)
  val initState = (market, memory)
  val backtest = BackTest[MockBrain.Memory, IO](brain)
  val reports = 
    Stream[IO, Chart](charts*)
      .through[IO, Either[Throwable, (Vector[Report], (BTMarket, MockBrain.Memory))]](backtest(initState))
      .collect{ case Right((reports, _)) => reports}
      .compile
      .foldMonoid
      .unsafeRunSync()
      
  println("REPORT !!!!!!!!!!!!!!!!!!!!!!!!" + reports)

