package com.github.imomushi8.systra.brain

import cats._
import cats.data.{StateT}
import cats.implicits.catsSyntaxFlatMapOps
import cats.effect.IO
import cats.implicits.catsSyntaxOptionId

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.Actions._
import com.github.imomushi8.systra.behavior._

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
        if (memory.chartList.size > previousDay)
          Memory(memory.chartList.takeRight(previousDay) :+ chart)
        else Memory(memory.chartList :+ chart)

      val size = 1
      val expire = chart.datetime.plusMonths(1)
      
      if context.positions.nonEmpty then
        val ref = for
          refMarket <- context.getMarket
          id  <- refMarket.placeOrder(MARKET(SELL, positionId = context.positions.head.id), size, expire)
          //oco <- IO { OCO(LIMIT(SELL, chart.close * 1.001, id), STOP(SELL, chart.close * 0.999, id)) }
          //_   <- refMarket.placeOrder(oco, size, expire)
        yield refMarket
        Actions.nextHandleErrorWith(newMemory, context, ref)
      else
        val ref = for
          refMarket <- context.getMarket 
          _ <- refMarket.placeOrder(LIMIT(BUY, 3), size, expire)
        yield refMarket
        Actions.nextHandleErrorWith(newMemory, context, ref)
/*
      // ポジションがある場合のみ注文する
      if (context.positions.nonEmpty) {
        val isUp = newMemory.chartList.head.close > newMemory.chartList.head.open
        val size = 200
        val expire = chart.datetime.plusMonths(1)

        // previousDay日前の値動きが上昇なら買い、下落なら売りでIFDOC
        val test = (if (isUp) for {
           id  <- context.placeOrder(MARKET(BUY, positionId = context.positions.head.id), size, expire)
           //oco <- Actions.io{ OCO(LIMIT(SELL, chart.close * 1.001, id), STOP(SELL, chart.close * 0.999, id)) }
           //_   <- context.placeOrder(oco, size, expire)
         } yield ()
         else
           for {
             id  <- context.placeOrder(MARKET(SELL, positionId = context.positions.head.id), size, expire)
             //oco <- Actions.io{ OCO(LIMIT(BUY, chart.close * 0.999, id), STOP(BUY, chart.close * 1.001, id)) }
             //_   <- context.placeOrder(oco, size, expire)
           } yield ()).value.unsafeRunSync()

      } else {
        if(context.orders.nonEmpty) then 
          context.cancelOrder(context.orders.head.id).value.unsafeRunSync()
      }
*/
  }
