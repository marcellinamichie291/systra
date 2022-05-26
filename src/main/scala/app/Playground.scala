package app

import app.Envs._
import app.backend._
import app.model._
import app.model.service._

import cats.implicits._
import cats.effect._
import cats.effect.unsafe.implicits.global

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
    _      <- IO.asyncForIO.start(useServer(status))
  //  _      <- IO.asyncForIO.foreverM(status.get >>= { s => IO.whenA(!s.isIdled) { IO.println(s) >> status.set(Idle) } })
    _      <- IO.asyncForIO.foreverM(status.get >>= { s => IO.println(s) })
  yield ExitCode.Success

  def useServer(statusRef: StatusRef[Service]): IO[Unit] = for
    host   <- LOCAL_HOST.load[IO]
    port   <- PORT.load[IO]
    ?      <- HttpBackend.getServer(host, port)(statusRef).useForever
  yield ()