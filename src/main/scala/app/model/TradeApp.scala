package app.model

import app.model.service._

import cats._
import cats.implicits._
import cats.data.Kleisli
import cats.effect._

import fs2.concurrent.SignallingRef

object TradeApp:
  def start(statusRef: StatusRef[Service]): IO[Unit] = statusRef
    .get
    .flatMap {_
      .map { _.getApp.run(statusRef) } // AppStatusがRunだったら中に入っているServiceを実行
      .getOrElse(IO.unit)              // それ以外ならなにもしない
    }
    .>> { statusRef.set(Idle) } // 終了したらIdle状態にする
