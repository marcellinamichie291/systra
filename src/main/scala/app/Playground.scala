package app

import app.model._
import app.model.service._

import cats.implicits._
import cats.effect._

import fs2._
import fs2.concurrent.{Topic, SignallingRef}
import fs2.concurrent.Topic.Closed
import com.comcast.ip4s._

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import collection.JavaConverters._

import java.util.Locale
import java.io.File
import java.io.FileWriter
import java.io.FileInputStream

object Playground extends IOApp:
  override def run(args: List[String]): IO[ExitCode] = for
    status <- SignallingRef[IO, AppStatus[Service]](Idle)
    _      <- Async[IO].start(app.backend.HttpBackend.getServer(status, ipv4"0.0.0.0", port"33415").useForever)
    _      <- IO.asyncForIO.foreverM(status.get >>= { s => IO.whenA(!s.isIdled) { IO.println(s) >> status.set(Idle) } } )
  yield
    ExitCode.Success