package app.model

import app.model.service._

import cats._
import cats.implicits._
import cats.data.Kleisli
import cats.effect._

import fs2.concurrent.SignallingRef

object TradeApp:
  def start(status: SignallingRef[IO, AppStatus[Service]]): IO[Unit] = status
    .get
    .flatMap { s => s.map(_.getApp.run(status)).getOrElse(IO.unit) }