package com.github.imomushi8.systra

import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.backtest.{BackTest, BTMarket}
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain

import cats.effect._
import cats.kernel.Monoid
import cats.implicits.catsSyntaxEitherId

import fs2._
import fs2.io.file.{Files, Path}

object SystemTrade:
  /** ダウンサンプリングを行う
   * TODO: 任意の時間間隔でのダウンサンプリングを可能にする
   */
  def downSampling(head:Chart):Pipe[IO, Chart, Chart] = _
    .scan(head.asLeft[(Chart, Chart)]) {
      // Leftであれば更新していく
      case (Left(Chart(open, high, low, close, volume, datetime)), chart) =>
        if (chart.datetime compareTo datetime.plusMinutes(5)) < 0 then // 規定時間内の場合
          Chart(open, high max chart.high, low min chart.low, chart.close, volume+chart.volume, datetime).asLeft[(Chart, Chart)]
        else
          (Chart(open, high, low, close, volume, datetime), chart).asRight[Chart] 

      // Rightであればリセットする
      case (Right(_, prev), Chart(_, high, low, close, volume, _)) =>
        Chart(prev.open, high max prev.high, low min prev.low, close, volume+prev.volume, prev.datetime).asLeft[(Chart, Chart)]
    }
    .collect { case (Right((chart, _))) => chart}

  def backtest[Memory:Initial](brains:       Seq[(String, Brain[BTMarket, Memory])],
                               firstCapital: Price,
                               firstChart:   Chart) =
    given initMarket: Initial[BTMarket] = BTMarket.initial(firstCapital, firstChart)
    val pipes = brains.map { case (name, brain) => (chartStream: Stream[IO, Chart]) => 
      chartStream
        .through(BackTest(brain))
        .through(BackTest.make(name, firstCapital))
        .map(_.toString)
    }
    (chartStream: Stream[IO, Chart]) => chartStream.broadcastThrough(pipes*)