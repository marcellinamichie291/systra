package app.model.service

import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.virtual.VirtualMarket._

import app.Envs._
import app.demo._
import app.bitflyer._
import app.backtest._
import app.backtest.report._

import cats.data.Kleisli
import cats.syntax.all._
import cats.implicits._
import cats.effect._

import fs2._
import fs2.concurrent.SignallingRef
import sttp.client3.Response

import scala.concurrent.duration.DurationInt
import java.time.temporal.TemporalAmount
import com.github.imomushi8.systra.chart.WebSocketStream
import app.model.AppStatus
import com.typesafe.scalalogging.LazyLogging
import fs2.concurrent.Signal

class DemoTradeService[Memory: Initial](brains:       Seq[(String, Brain[VirtualMarket, Memory])], 
                                        firstCapital: Price,
                                        ws:           WebSocketStream) extends Service with LazyLogging:
  override def getApp: Kleisli[IO, StatusRef[Service], Unit] = Kleisli { status => 
    val trade: Pipe[IO, Chart, Unit] = _.through(Demo(brains, firstCapital))
    begin(ws(trade), status).end() 
  }

  /** 開始メソッド */
  def begin(ws:        Kleisli[IO, Signal[IO, Boolean], Unit], 
            statusRef: StatusRef[Service]): Ops = new Ops(ws, statusRef)
 
  class Ops(ws:        Kleisli[IO, Signal[IO, Boolean], Unit], 
            statusRef: StatusRef[Service]):

    /** 終了メソッド */
    def end() = ws
      .run(statusRef.map(_.isIdled))
      .void
      .handleErrorWith { t =>
        logger.error("Next error occured", t)
        IO.println("Done DemoTrade")
      }