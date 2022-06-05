package app.model.service

import app.model.AppStatus

import cats.data.Kleisli
import cats.effect.IO

import fs2.concurrent.SignallingRef

type StatusRef[S] = SignallingRef[IO, AppStatus[S]]

trait Service:
  def getApp: Kleisli[IO, StatusRef[Service], Unit]