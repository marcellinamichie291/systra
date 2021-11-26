package com.github.imomushi8.systra.backtest

import cats.effect.{ExitCode, IO}
import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.behavior.MarketBehavior
import com.typesafe.scalalogging.LazyLogging

case class BTMarket(capital     :Double,
                    orders      :List[Order],
                    positions   :List[Position],
                    sequenceId  :Int,
                    chart       :Chart,
                    count       :Int): // TODO: fs2/Streamのメソッドでカウント関数があるので、もしかしたら削除可能かも
  override val toString:String = 
    f"""===================================VIEW STATE INFO===================================
       |BackTest Market
       |  Capital      : $capital%.3f
       |  Orders       : $orders
       |  Positions    : $positions
       |  Sequential ID: $sequenceId
       |  chart        : $chart
       |  count        : $count
       |=====================================================================================""".stripMargin('|')


object BTMarket extends LazyLogging:
  inline val NEXT_STATE ="{}: Next State\n{}"

  given MarketBehavior[BTMarket] with
    override def placeOrder(current: BTMarket, method: OrderMethod, size: Size, expire: TimeStamp): IO[(BTMarket, ID)] = IO {
      /* sizeの入力が不正だったら例外を投げる */
      if size <= 0 then
        val errMsg = "Your input size is non-positive. Please input positive numeric"
        logger.warn(errMsg)
        throw new SizeNegativeException(errMsg)
      
      val newOrders = makeOrder(current.chart, current.sequenceId, method, size, expire)

      // FIXME: STOP_LIMITのときに資金が2重に減ってしまう現象と、STOPのときに資金が減らない問題がある
      val nextCapital = current.capital - newOrders.map {order => order.price*order.size}.sum

      /* 発注後の資金がマイナスなら例外を投げる */
      if nextCapital < 0 then 
        val errMsg = "Your capital does not enough. So couldn't place order"
        logger.warn(errMsg)
        throw new CapitalShortfallException(errMsg)

      newOrders.foreach { order => logger.debug(OPEN, order) }

      val next = current.copy(
        capital    = nextCapital,
        orders     = current.orders++newOrders,
        sequenceId = current.sequenceId + newOrders.size)
      
      //logger.debug(NEXT_STATE, "Place Order", next)
      (next, newOrders.head.id)
    }

    override def cancelOrder(current: BTMarket, id: ID): IO[(BTMarket, ExitCode)] =
      current.orders.find(_.id == id).map { order => IO {
          logger.debug(CANCEL, order)
          val next = current.copy(orders = current.orders.filterNot(_.id == id)) // FIXME: 親・兄弟注文を削除できるようにしたい
          //logger.debug(NEXT_STATE, "Cancel Order", next)
          (next, ExitCode.Success)
        }
      } getOrElse IO {
        val errMsg = s"Order(ID: $id) is not found. Maybe it has already closed or canceled."
        logger.warn(errMsg)
        //logger.debug(NEXT_STATE, "Cancel Order", current)
        throw new OrderCancelFailureException(errMsg)
      }

    override def updateChart(current: BTMarket, chart: Chart): BTMarket =
      val next = current.copy(chart=chart, count=current.count+1)
      //logger.debug(NEXT_STATE, "Update Chart", next)
      next

    override def checkContract(current: BTMarket): IO[(BTMarket, Vector[Report])] = current match 
      case BTMarket(capital, orders, positions, sequenceId, chart, count) =>
        IO {
          /* 有効期限切れの注文があった場合は削除する */
          val nonExpiredOrders = orders filter { order => (chart.datetime compareTo order.expire) < 0 }
          (orders filter { order => (chart.datetime compareTo order.expire) >= 0 }) foreach {
            order => logger.warn(s"$order is expired.") 
          }

          /* Order, Positionそれぞれについて削除・追加するものを取得する */
          val (nextOrders, nextPositions, reports) = checkAllContract(chart, nonExpiredOrders, positions)
        
          val pl = (reports map {
            case PositionReport(_, _, side, size, openPrice, closePrice, cost) => 
              (side*(closePrice - openPrice))*size + cost
            case _ => 0 // ありえないが、exhaustingをなくすため
          }).sum
          
          val next = BTMarket(capital + pl, nextOrders, nextPositions, sequenceId, chart, count)
          logger.debug(NEXT_STATE, "Check Contract", next)
          (next, reports)
        }

    override def getContext(market: BTMarket): MarketContext[BTMarket] = market match
      case BTMarket(capital, orders, positions, _, _, _) => MarketContext(capital, orders, positions, market)