package app.backend

import app.model.AppStatus
import app.model.service._

import cats.effect._

import fs2.concurrent.SignallingRef

import com.comcast.ip4s._
import org.http4s.ember.server._
import org.http4s.implicits._
import org.http4s.server.Router

object HttpBackend:
  def getServer(host:      Ipv4Address,
                port:      Port)
               (statusRef: StatusRef[Service]) =
    val httpApp = Router("/api" -> Routes(statusRef))
      .orNotFound

    EmberServerBuilder
      .default[IO]
      .withHost(host)
      .withPort(port)
      .withHttpApp(httpApp)
      .build