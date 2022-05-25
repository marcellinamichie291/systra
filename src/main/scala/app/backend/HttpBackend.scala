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
  def getServer(status: SignallingRef[IO, AppStatus[Service]], host: Ipv4Address, port: Port) = 
    val routes = Routes(status)
    val httpApp = Router("/api" -> routes.services).orNotFound
    
    EmberServerBuilder
      .default[IO]
      .withHost(host)
      .withPort(port)
      .withHttpApp(httpApp)
      .build