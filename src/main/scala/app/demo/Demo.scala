package app.demo

import com.github.imomushi8.systra.chart._
import com.github.imomushi8.systra.virtual.VirtualMarket
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.market._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.util.Initial

import cats.effect.IO
import fs2._
import fs2.concurrent.SignallingRef
import app.model.AppStatus
import app.model.service.Service

object Demo extends Tradable[VirtualMarket]:

  /*
   * TODO: 実装
   *       ここでtrade実行を行い、受け取ったPositionTransactionを処理する（DBに入れる or htmlに書き下す）(後者を想定中)
   */
  def apply[Memory: Initial](
      brains: Seq[(String, Brain[VirtualMarket, Memory])],
      firstCapital: Price): Pipe[IO, Chart, Unit] = chartStream =>
    chartStream
      .head
      .flatMap { head => 
        chartStream
          .through(_.printlns)
          .drain 
      }
