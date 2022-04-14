package com.github.imomushi8.systra.core.data

import com.github.imomushi8.systra.core.entity.Chart

import cats.effect.IO
import cats.kernel.Monoid
import cats.implicits.catsSyntaxEitherId

import fs2.{Stream, Pipe}

trait ChartStream:
  /** ChartのStreamを開始するメソッド */
  def begin(): Stream[IO, Chart]


object ChartStream: 
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