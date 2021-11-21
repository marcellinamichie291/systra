package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.behavior.Tradable
import com.github.imomushi8.systra.behavior.OrderManagementSystem

/** Tradableのバックテスト用インスタンス */
object BackTest extends OrderManagementSystem[BTMarket]//Tradable[BTMarket]