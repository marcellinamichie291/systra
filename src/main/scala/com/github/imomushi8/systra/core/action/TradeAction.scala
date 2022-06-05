package com.github.imomushi8.systra.core.action

sealed trait TradeAction[Market, Memory]
case class Next[Market, Memory](market: Market, memory: Memory) extends TradeAction[Market, Memory]
case class End[Market, Memory](endMsg: String) extends TradeAction[Market, Memory]