package com.github.imomushi8.systra.behavior

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.entity._
import com.github.imomushi8.systra.report._

import cats.effect.{ExitCode, IO}

trait MarketBehavior[Market]:

  def placeOrder(current: Market,
                 method:  OrderMethod,
                 size:    Size,
                 expire:  TimeStamp): IO[(Market, ID)]

  def cancelOrder(current: Market,
                  id: ID): IO[(Market, ExitCode)]

  def updateChart(current: Market,
                  chart:   Chart): Market

  def checkContract(current: Market): IO[(Market, Vector[PositionReport])]

  def getContext(market: Market): MarketContext[Market]

  extension (market: Market)
    def <<(method: OrderMethod,
           size:   Size,
           expire: TimeStamp): IO[(Market, ID)] = placeOrder(market, method, size, expire)
    def >>(id: ID): IO[(Market, ExitCode)] = cancelOrder(market, id)
    def :=(chart: Chart): Market = updateChart(market, chart)
    def contract: IO[(Market, Seq[PositionReport])] = checkContract(market)
    def context: MarketContext[Market] = getContext(market)