package com.github.imomushi8.systra.backtest

import cats.effect.{ExitCode, IO}
import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.entity._
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.behavior.MarketBehavior
import com.typesafe.scalalogging.LazyLogging

case class BTMarket(capital     :Double,
                    orders      :List[Order],
                    positions   :List[Position],
                    sequenceId  :Int,
                    chart       :Chart):
  override val toString:String = 
    f"""===================================VIEW STATE INFO===================================
       |BackTest Market
       |  Capital      : $capital%.3f
       |  Orders       : $orders
       |  Positions    : $positions
       |  Sequential ID: $sequenceId
       |  chart        : $chart
       |=====================================================================================""".stripMargin('|')


object BTMarket extends LazyLogging:
  inline val NEXT_STATE ="Next State\n{}"

  given MarketBehavior[BTMarket] with
    override def placeOrder(current: BTMarket, method: OrderMethod, size: Size, expire: TimeStamp): IO[(BTMarket, ID)] = IO {
      logger.debug(START, "Place Order")
      /* sizeの入力が不正だったら例外を投げる */
      if size <= 0 then
        val errMsg = f"""Your input size is non-positive. Please input positive numeric
                        |size : ${size}%0.3f""".stripMargin('|')
        logger.warn(errMsg)
        throw new SizeNegativeException(errMsg)
      
      val newOrders = makeOrder(current.chart, current.positions, current.sequenceId, method, size, expire)

      // 注文に伴う資金減少
      val cost = newOrders.map { order => 
        if order.isLIMIT then order.side*order.price*order.size else order.side*order.triggerPrice*order.size }.sum
      val nextCapital = current.capital - cost

      /* 発注後の資金がマイナスなら例外を投げる */
      if nextCapital < 0 then 
        val errMsg = f"""Your capital does not enough. So couldn't place order
                        |capital    : ${current.capital}%0.3f
                        |order cost : $cost%0.3f""".stripMargin('|')
        logger.warn(errMsg)
        throw new CapitalShortfallException(errMsg)

      newOrders.foreach { order => logger.debug(OPEN, order) }

      val next = current.copy(
        capital    = nextCapital,
        orders     = current.orders++newOrders,
        sequenceId = current.sequenceId + newOrders.size)
      
      logger.debug(NEXT_STATE, next)
      logger.debug(END, "Place Order")
      (next, newOrders.head.id)
    }

    override def cancelOrder(current: BTMarket, id: ID): IO[(BTMarket, ExitCode)] =
      logger.debug(START, "Cancel Order")
      current.orders.find(_.id == id).map { order => IO {
          logger.debug(CANCEL, order)
          val next = current.copy(orders = current.orders.filterNot { order =>
            order.id == id || order.parentId == id || order.brotherId == id // 親・兄弟注文を削除
          }) 
          logger.debug(NEXT_STATE, next)
          logger.debug(END, "Cancel Order")
          (next, ExitCode.Success)
        }
      } getOrElse IO {
        val errMsg = s"Order(ID: $id) is not found. Maybe it has already closed or canceled."
        logger.warn(errMsg)
        logger.debug(NEXT_STATE, current)
        logger.debug(END, "Cancel Order")
        throw new OrderCancelFailureException(errMsg)
      }

    override def updateChart(current: BTMarket, chart: Chart): BTMarket =
      logger.debug(START, "Update Chart")
      val next = current.copy(chart=chart)
      logger.debug(NEXT_STATE, next)
      logger.debug(END, "Update Chart")
      next

    override def checkContract(current: BTMarket): IO[(BTMarket, Vector[Report])] = current match 
      case BTMarket(capital, orders, positions, sequenceId, chart) => IO {
        logger.debug(START, "Check Contract")

        /* 有効期限切れの注文があった場合は削除する */
        val expiredOrders = orders filter { order => (chart.datetime compareTo order.expire) >= 0 }
        val nonExpiredOrders = orders diff expiredOrders
        val cancelPl = expiredOrders.map{ order => 
          if order.isLIMIT then order.side*order.price*order.size else order.side*order.triggerPrice*order.size
        }.sum

        //expiredOrders.foreach{order => logger.warn(s"$order(Expire: ${order.expire}) is expired in ${chart.datetime}")}

        /* Order, Positionそれぞれについて削除・追加するものを取得する */
        val (nextOrders, nextPositions, reports) = checkAllContract(chart, nonExpiredOrders, positions)
      
        val pl = (reports map {
          case PositionReport(_, _, side, size, openPrice, closePrice, cost) => 
            side*closePrice*size + cost // 
          case _ => 0 // ありえないが、exhaustingをなくすため
        }).sum
        
        val next = BTMarket(capital + pl + cancelPl, nextOrders, nextPositions, sequenceId, chart)
        logger.debug(NEXT_STATE, next)
        logger.debug(END, "Check Contract")
        (next, reports)
      }

    override def getContext(market: BTMarket): MarketContext[BTMarket] = market match
      case BTMarket(capital, orders, positions, _, _) => MarketContext(capital, orders, positions, market)