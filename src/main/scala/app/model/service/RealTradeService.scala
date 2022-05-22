package app.model.service

import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity.Chart
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

import scala.concurrent.duration.DurationInt
import java.time.temporal.TemporalAmount
import com.github.imomushi8.systra.chart.WebSocketStream
import app.model.AppStatus

class RealTradeService(ws: WebSocketStream) extends Service:
  override def getApp: Kleisli[IO, SignallingRef[IO, AppStatus[Service]], Unit] = Kleisli { signal => ??? }
