package com.github.imomushi8.systra.backtest

import cats.data.State
import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util.Position
import com.github.imomushi8.systra.report.Report
import com.github.imomushi8.systra.behavior.{MarketBehavior, Tradable}
import com.typesafe.scalalogging.LazyLogging

object BackTest extends Tradable[BTMarket] with LazyLogging:
  override def contract[Memory]: State[(BTMarket, Memory), Seq[Report]] =
    State { case (BTMarket(capital, orders, positions, sequenceId, chart, count), memory) =>
    
      /* Order, Positionそれぞれについて削除・追加するものを取得する */
      val (deleteOrders, newOrders, deletePositions, newPositions) = checkAllContract(chart, orders, positions)
      val nextOrders    = orders.diff(deleteOrders) ++ newOrders
      val nextPositions = positions.diff(deletePositions.map(_._1)) ++ newPositions

      logger.trace(s"GET $chart")
      logger.trace(s"""${chart.datetime}: Orders    => ${nextOrders.mkString(",")}""")
      logger.trace(s"""${chart.datetime}: Positions => ${nextPositions.mkString(",")}""")
    
      val pl = deletePositions.map { case (position, closePrice, _) =>
        (position.side*(closePrice - position.price))*position.size
      }.sum

      val next = BTMarket(capital + pl, nextOrders, nextPositions, sequenceId, chart, count)
      ((next, memory), makeReport(deletePositions))
    }
    
  override def getContext(market: BTMarket): MarketContext[BTMarket] = market match
    case BTMarket(capital, orders, positions, _, _, _) => 
      MarketContext(capital, orders, positions, market)
