package com.github.imomushi8.systra.core.market

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._

import cats.effect.{ExitCode, IO}

/**
 * 新たな市場を定義した場合は実装が必要な型クラス
 */
trait MarketBehavior[Market]:

  def placeOrder(current: Market,
                 method:  OrderMethod,
                 size:    Size,
                 expire:  TimeStamp): IO[(Market, ID)]

  def cancelOrder(current: Market,
                  id: ID): IO[(Market, ExitCode)]

  def updateChart(current: Market,
                  chart:   Chart): Market

  def checkContract(current: Market): IO[(Market, Vector[PositionTransaction])]

  def getContext(market: Market): MarketContext[Market]

  extension (market: Market)
    def <<(method: OrderMethod,
           size:   Size,
           expire: TimeStamp): IO[(Market, ID)] = placeOrder(market, method, size, expire)
    def >>(id: ID): IO[(Market, ExitCode)] = cancelOrder(market, id)
    def :=(chart: Chart): Market = updateChart(market, chart)
    def contract: IO[(Market, Seq[PositionTransaction])] = checkContract(market)
    def context: MarketContext[Market] = getContext(market)