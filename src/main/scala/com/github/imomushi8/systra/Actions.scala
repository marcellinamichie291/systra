package com.github.imomushi8.systra

import com.github.imomushi8.systra.util.Chart

object Actions {
  type Brain[Market, Memory] = (Chart, MarketContext[Market], Memory) => (Market, Memory)

  def receive[Market, Memory](brain: Brain[Market, Memory]): Brain[Market, Memory] = brain

  def next[Market, Memory](memory: Memory, context: MarketContext[Market]): (Market, Memory) =
    (context.market, memory)
}