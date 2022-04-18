package com.github.imomushi8.systra.demo

import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.data.ChartStream
import com.github.imomushi8.systra.core.market.{Tradable, OK, TradeState}
import com.github.imomushi8.systra.backtest.BTMarket

import cats.implicits._
import cats.effect._
import fs2._
import fs2.concurrent._
import fs2.io.file.{Files, Path}

import sttp.model.Uri
import sttp.client3._
import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend
import sttp.capabilities.fs2.Fs2Streams
import sttp.ws.{WebSocket, WebSocketFrame}


trait Demo[Memory: Initial](brains:       Seq[(String, Brain[BTMarket, Memory])],
                            firstCapital: Price,
                            uri:          Uri) extends Tradable[BTMarket]:

  def wsPipe: Pipe[IO, WebSocketFrame.Data[?], WebSocketFrame]

  def summerize[Memory:Initial](brains:       Seq[(String, Brain[BTMarket, Memory])],
                                firstCapital: Price,
                                firstChart:   Chart) =
    val initSub = SummarySubReport(0,0,0,0,0)
    val initSummary = SummaryReport("", 0, 0, 0, OK, initSub, initSub, initSub, 0, 0, 0)
    given initMarket: Initial[BTMarket] = BTMarket.initial(firstCapital, firstChart)
    
    brains.map { case (name, brain) => (chartStream: Stream[IO, Chart]) => 
      chartStream
        .through(trade(brain))
        .fold(initSummary) { 
          case (current, (_, _, TradeState(sampleSize, flag, buy, sell, capital, maxCapital, consWins, consLoses, _))) =>
            val buySub = SummarySubReport(buy.winCount, buy.loseCount, buy.profit, buy.loss, buy.cost)
            val sellSub = SummarySubReport(sell.winCount, sell.loseCount, sell.profit, sell.loss, sell.cost)
            val maximalDrawDown = current.maximalDrawDown max (maxCapital - capital)
            val consecutiveWinCount  = current.consecutiveWinCount max consWins
            val consecutiveLoseCount = current.consecutiveLoseCount max consLoses
            SummaryReport(name, sampleSize, firstCapital, capital, flag, buySub, sellSub, buySub+sellSub, maximalDrawDown, consecutiveWinCount, consecutiveLoseCount)
        }
    }

  /** Demoの開始メソッド */
  def begin(): DemoOps = new DemoOps
 
  class DemoOps:
    /** Demoの終了メソッド */
    def end() = AsyncHttpClientFs2Backend
      .resource[IO]()
      .use { backend =>
        basicRequest
          .response(asWebSocketStreamAlways(Fs2Streams[IO])(wsPipe))
          .get(uri)
          .send(backend)
      }
      .>> { IO.println("Done DemoTrade") }