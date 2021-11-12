package com.github.imomushi8.systra

sealed trait TradeAction[Market, Memory]
case class Next[Market, Memory](market: Market, memory: Memory) extends TradeAction[Market, Memory]
case class End[Market, Memory](t:Throwable) extends TradeAction[Market, Memory]