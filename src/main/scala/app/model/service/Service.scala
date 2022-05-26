package app.model.service

import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity.Chart
import com.github.imomushi8.systra.virtual.VirtualMarket._

import app.Envs._
import app.demo._
import app.bitflyer._
import app.backtest._

import cats.data.Kleisli
import cats.syntax.all._
import cats.implicits._
import cats.effect._

import fs2._
import fs2.concurrent.SignallingRef

import sttp.ws.WebSocketFrame
import com.typesafe.scalalogging.LazyLogging
import java.time.temporal.TemporalAmount
import scala.concurrent.duration.DurationInt

import app.model.AppStatus

type StatusRef[S] = SignallingRef[IO, AppStatus[S]]

trait Service:
  def getApp: Kleisli[IO, StatusRef[Service], Unit]