package app.demo

import com.github.imomushi8.systra.chart._
import com.github.imomushi8.systra.virtual.VirtualMarket
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.market._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.util.Initial

import cats.effect.IO
import fs2._

object Demo extends Tradable[VirtualMarket]:
  def summerize[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])],
                                 firstCapital: Price,
                                 firstChart:   Chart): Pipe[IO, Chart, Unit] = _.println

  /** 開始メソッド */
  def begin(ws: WebSocketStream): Ops = new Ops(ws)
 
  class Ops(ws: WebSocketStream):
    /** Demoの終了メソッド */
    def end() = ws() >> { IO.println("Done DemoTrade") }