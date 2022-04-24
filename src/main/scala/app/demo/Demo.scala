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

object Demo extends Tradable[VirtualMarket]:

  def apply[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])],
                             firstCapital: Price,
                             firstChart:   Chart): Pipe[IO, Chart, Unit] = _.printlns

  /** 開始メソッド */
  def begin(ws: WebSocketStream, haltOnSignal: SignallingRef[IO, Boolean]): Ops = new Ops(ws, haltOnSignal)
 
  class Ops(ws: WebSocketStream, haltOnSignal: SignallingRef[IO, Boolean]):
    /** 終了メソッド */
    def end() = ws(haltOnSignal) >> { IO.println("Done DemoTrade") }