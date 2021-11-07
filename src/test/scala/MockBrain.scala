import cats._
import cats.effect.IO
import cats.effect.unsafe.implicits.global

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.behavior._
import com.github.imomushi8.systra.Actions._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.MarketContext._

object MockBrain:
  case class Memory(chartList: List[Chart])

  given Monoid[Memory] with
    override def empty: Memory = Memory(Nil)
    override def combine(x: Memory, y: Memory): Memory = Memory(
      x.chartList ++ y.chartList
    )

  def apply[Market](previousDay: Int)
                   (using MarketBehavior[Market]): Brain[Market, Memory] = Actions.receive {
    (chart, context, memory) =>
      val newMemory =
        if (memory.chartList.size > previousDay + 1)
          Memory(memory.chartList.takeRight(previousDay + 1) :+ chart)
        else Memory(memory.chartList :+ chart)

      /* ポジションがない場合のみ注文する */
      if (context.positions.isEmpty) {
        val isUp = newMemory.chartList.head.close > newMemory.chartList.head.open
        val size = 200
        val expire = chart.datetime.plusMonths(1)

        /** previousDay日前の値動きが上昇なら買い、下落なら売りでIFDOCO */
        (if (isUp) for {
           id <- context.placeOrder(MARKET(BUY), size, expire)
           oco <- IO{ OCO(LIMIT(SELL, chart.close * 1.001, id), STOP(SELL, chart.close * 0.999, id)) }
           _ <- context.placeOrder(oco, size, expire)
         } yield ()
         else
           for {
             id <- context.placeOrder(MARKET(SELL), size, expire)
             oco <- IO{ OCO(LIMIT(BUY, chart.close * 0.999, id), STOP(BUY, chart.close * 1.001, id)) }
             _ <- context.placeOrder(oco, size, expire)
           } yield ()).unsafeRunSync()

      } else {
        context.cancelOrder(context.orders.head.id).unsafeRunSync()
      }

      Actions.next(newMemory, context)
  }
