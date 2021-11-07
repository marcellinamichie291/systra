package com.github.imomushi8.systra.backtest

import cats.effect.{ExitCode, IO}
import com.github.imomushi8.systra._
import com.github.imomushi8.systra.behavior.MarketBehavior
import com.github.imomushi8.systra.util.{Chart, Order, OrderMethod, Position}
import com.github.imomushi8.systra.{ID, Size, TimeStamp}
import com.typesafe.scalalogging.LazyLogging

case class BTMarket(capital     :Double,
                    orders      :List[Order],
                    positions   :List[Position],
                    sequenceId  :Int,
                    chart       :Chart,
                    count       :Int)

object BTMarket extends LazyLogging:

  given MarketBehavior[BTMarket] with
    override def placeOrder(current: BTMarket, method: OrderMethod, size: Size, expire: TimeStamp): IO[(BTMarket, ID)] = IO {
      val newOrders = BackTestProcedure.getOrders(current.chart, current.sequenceId, method, size, expire)
      val next = current.copy(
        orders = current.orders++newOrders,
        sequenceId = current.sequenceId + newOrders.size)
      (next, newOrders.head.id)
    }

    override def cancelOrder(current: BTMarket, id: ID): IO[(BTMarket, ExitCode)] = IO {
      if (current.orders.exists(_.id == id)) {
        val order = current.orders.find(_.id == id).get
        logger.trace(CANCEL, order)
        val next = current.copy(orders = current.orders.filterNot(_.id == id))
        (next, ExitCode.Success)

      } else {
        logger.trace(CANCEL, s"Order($id) FAILURE. Maybe it has already closed or canceled.")
        (current, ExitCode.Error)
      }
    }

    override def updateChart(current: BTMarket, chart: Chart): BTMarket = 
      current.copy(chart=chart, count=current.count+1)