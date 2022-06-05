package app

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.virtual._
import app.brain._

import cats.effect.IO
import ciris._
import sttp.client3._
import com.comcast.ip4s._

object Envs:
  val BITFLYER_API_KEY        = env("BITFLYER_API_KEY").secret
  val BITFLYER_API_SECRET     = env("BITFLYER_API_SECRET").secret
  val BITFLYER_PUBLIC_CHANNEL = default("lightning_executions_BTC_JPY")

  val LOCAL_HOST = default(ipv4"0.0.0.0")
  val PORT       = default(port"33415")

  type M = ControlChartBrain.Memory

  val brainName = "ControlChartBrain"
  val brains    = {
    for
      t         <- 24 to 96 by 24
      maxBias   <- 10 to 55 by 15
      maxUpDown <- 10 to 55 by 15
      maxScat   <- 30 to 50 by 20
    yield (
      s"$brainName($t $maxBias $maxUpDown $maxScat)",
      ControlChartBrain[VirtualMarket](t, maxBias, maxUpDown, maxScat))
    // List((s"$brainName(36 8 8 15)", ControlChartBrain[VirtualMarket](36, 8, 8, 15)))
    // for t <- 36 until 108 by 6 yield (s"$brainName($t 8 8 15)", ControlChartBrain[VirtualMarket](t, 8, 8, 15))
  }

  val firstCapital     = 1_000_000.0 // 100万円
  val leverage         = 25.0        // レバレッジ
  val leveragedCapital = firstCapital * leverage

  val readCsvPath  = "csv_chart/USDJPY_2015_2021/USDJPY_2016_all.csv"
  val writeCsvPath = s"reports/${brainName}_2016_5.csv"
