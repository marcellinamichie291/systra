package com.github.imomushi8.systra

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.entity._
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.backtest.{BackTest, BTMarket}

import cats.effect._
import cats.kernel.Monoid
import cats.implicits.catsSyntaxEitherId

import fs2._
import fs2.io.file.{Files, Path}

object SystemTrade:
  def downSampling(head:Chart):Pipe[IO, Chart, Chart] = _
    .scan(head.asLeft[(Chart, Chart)]) {
      // Leftであれば更新していく
      case (Left(Chart(open, high, low, close, volume, datetime)), chart) =>
        if (chart.datetime compareTo datetime.plusMinutes(5)) < 0 then // 規定時間内（今回は1時間）の場合
          Chart(open, high max chart.high, low min chart.low, chart.close, volume+chart.volume, datetime).asLeft[(Chart, Chart)]
        else
          (Chart(open, high, low, close, volume, datetime), chart).asRight[Chart] 

      // Rightであればリセットする
      case (Right(_, prev), Chart(_, high, low, close, volume, _)) =>
        Chart(prev.open, high max prev.high, low min prev.low, close, volume+prev.volume, prev.datetime).asLeft[(Chart, Chart)]
    }
    .collect { case (Right((chart, _))) => chart}

  def backtestStream[Memory:Monoid](brains:       Seq[(String, Brain[BTMarket, Memory])],
                                    firstCapital: Price,
                                    firstChart:   Chart) =
    val initMarket = BTMarket(firstCapital, Nil, Nil, 1, firstChart)
    val initMemory = Monoid[Memory].empty
    val pipes = brains.map { case (name, brain) => (chartStream: Stream[IO, Chart]) => 
      chartStream
        .through(BackTest(brain, initMarket, initMemory))
        .zipWithIndex
        .flatMap { case ((reports, flag, capital) , index) => Stream( (reports.map{r=>(r.asInstanceOf[PositionReport], index, flag, capital)})* )}
        .through(BackTest.makeSummary(name, firstCapital))
        .map(_.toString)
    }

    (chartStream: Stream[IO, Chart]) => chartStream.broadcastThrough(pipes*)

