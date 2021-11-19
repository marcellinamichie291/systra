package com.github.imomushi8.systra.backtest

import cats.effect.{ExitCode, IO}
import com.github.imomushi8.systra._
import com.github.imomushi8.systra.behavior.MarketBehavior
import com.github.imomushi8.systra.util.{Chart, Order, OrderMethod, Position}
import com.github.imomushi8.systra.report._
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
      val newOrders = makeOrder(current.chart, current.sequenceId, method, size, expire)
      val nextCapital = current.capital - newOrders.map {order => order.price*order.size}.sum
      
      //logger.debug(ORDER, newOrders)
      
      /* 資金がマイナスなら例外 */
      if nextCapital > 0 then throw new RuntimeException("Your Capital does not enough. So cannot place order")

      val next = current.copy(
        capital    = nextCapital,
        orders     = current.orders++newOrders,
        sequenceId = current.sequenceId + newOrders.size)
      (next, newOrders.head.id)
    }

    override def cancelOrder(current: BTMarket, id: ID): IO[(BTMarket, ExitCode)] = IO {
      if current.orders.exists(_.id == id) then
        val order = current.orders.find(_.id == id).get
        //logger.debug(CANCEL, order)
        val next = current.copy(orders = current.orders.filterNot(_.id == id))
        (next, ExitCode.Success)
      else
        //logger.debug(CANCEL, s"Order($id) FAILURE. Maybe it has already closed or canceled.")
        throw new RuntimeException("Cancel Failure.")
        (current, ExitCode.Error)
    }

    override def updateChart(current: BTMarket, chart: Chart): BTMarket =
      logger.debug(GET, chart)
      current.copy(chart=chart, count=current.count+1)

    override def checkContract(current: BTMarket): IO[(BTMarket, Seq[Report])] = IO { current match 
      case BTMarket(capital, orders, positions, sequenceId, chart, count) =>
        /* Order, Positionそれぞれについて削除・追加するものを取得する */
        val (nextOrders, nextPositions, contractedPositions) = 
          checkAllContract(chart, orders, positions)

        //logger.trace(s"""${chart.datetime}: Orders    => ${nextOrders.mkString(",")}""")
        //logger.trace(s"""${chart.datetime}: Positions => ${nextPositions.mkString(",")}""")
      
        val pl = contractedPositions.map { case (position, closePrice, _) => 
          (position.side*(closePrice - position.price))*position.size
        }.sum

        val next = BTMarket(capital + pl, nextOrders, nextPositions, sequenceId, chart, count)
        (next, makeReport(contractedPositions))
    }

    override def getContext(market: BTMarket): MarketContext[BTMarket] = market match
      case BTMarket(capital, orders, positions, _, _, _) => 
        MarketContext(capital, orders, positions, market)