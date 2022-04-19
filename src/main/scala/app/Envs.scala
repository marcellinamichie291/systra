package app

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.backtest._
import app.brain._

import sttp.client3._

object Envs:
  val BITFLYER_WS_URL = uri"wss://ws.lightstream.bitflyer.com/json-rpc"
  val BITFLYER_API_KEY = "MxwMqUeSC1Nz9xByQ8Ujk3"
  val BITFLYER_API_SECRET = "F8rQyKwkzP4ZAO1Eu3+1SKOTgy73/kCHO5SB0JHaWHU="
  val BITFLYER_PUBLIC_CHANNELS = List("lightning_executions_BTC_JPY")


  type M = ControlChartBrain.Memory

  val brainName="ControlChartBrain"
  val brains = { for
      t <- 24 to 96 by 24
      maxBias <- 10 to 55 by 15
      maxUpDown <- 10 to 55 by 15
      maxScat <- 30 to 50 by 20
    yield (
      s"$brainName($t $maxBias $maxUpDown $maxScat)", 
      ControlChartBrain[BTMarket](t, maxBias, maxUpDown, maxScat))
    //List((s"$brainName(36 8 8 15)", ControlChartBrain[BTMarket](36, 8, 8, 15)))
    //for t <- 36 until 108 by 6 yield (s"$brainName($t 8 8 15)", ControlChartBrain[BTMarket](t, 8, 8, 15))
  }

  val firstCapital = 1_000_000.0 // 100万円
  val leverage = 25.0 // レバレッジ
  val leveragedCapital = firstCapital*leverage
  
  val readCsvPath = "csv_chart/USDJPY_2015_2021/USDJPY_2016_all.csv"
  val writeCsvPath = s"reports/${brainName}_2016_5.csv"
