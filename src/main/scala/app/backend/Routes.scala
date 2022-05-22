package app.backend

import app.demo.Demo
import app.model.AppStatus
import app.model.service._

import cats.effect._
import cats.syntax.all._

import fs2.concurrent.SignallingRef

import com.comcast.ip4s._
import org.http4s.ember.server._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.dsl.impl.QueryParamDecoderMatcher


case class Routes(signal: SignallingRef[IO, AppStatus[Service]]):
  object QueryMarket extends QueryParamDecoderMatcher[String]("market")
  object QueryPeriod extends QueryParamDecoderMatcher[String]("period")

  val services = startService <+> stopService
  
  val brains = app.Envs.brains
  val capital = app.Envs.leveragedCapital
  val readCsv = app.Envs.readCsvPath
  val writeCsv = app.Envs.writeCsvPath

  /* トレード開始 */
  def startService(using clock: Clock[IO]) = HttpRoutes.of[IO] {
    case GET -> Root / service / "start" :? QueryMarket(market) => service match
      case "backtest"  => 
        for
          _   <- signal.update (AppStatus._run(BackTestService[app.Envs.M](brains, capital, readCsv, writeCsv)))
          res <- Ok(s"BackTest Start")
        yield res

      case "demotrade" =>
        
        // このへんのパラメータもあとで取得する
        val wsFactory = WebSocketFactory(Demo(brains, capital), java.time.Duration.ofMillis(1L))

        for
          ws  <- wsFactory.get(market)
          ?   <- signal.update(AppStatus._run(DemoTradeService(ws))) 
          res <- Ok(s"DemoTrade Start")
        yield res

      case _           => BadRequest()
  }

  val stopService = HttpRoutes.of[IO] { 
    case GET -> Root / "stop" =>
      /* トレード終了 */
      signal.update { _.idle } >> Ok(s"Stop") 
  }
