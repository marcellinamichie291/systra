package com.github.imomushi8.systra.behavior

import cats.effect.{ExitCode, IO}
import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._

trait MarketBehavior[Market]:
  def placeOrder(current: Market,
                 method: OrderMethod,
                 size: Size,
                 expire: TimeStamp): IO[(Market, ID)]

  def cancelOrder(current: Market,
                  id: ID): IO[(Market, ExitCode)]

  def updateChart(current: Market,
                  chart: Chart): Market

  extension (market:Market)
    def <<(method: OrderMethod,
                   size: Size,
                   expire: TimeStamp): IO[(Market, ID)] = placeOrder(market, method, size, expire)
    def >>(id: ID): IO[(Market, ExitCode)] = cancelOrder(market, id)
    def :=(chart: Chart): Market = updateChart(market, chart)