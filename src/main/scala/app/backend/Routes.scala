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
    case GET -> Root / "start" / serviceName => serviceName match
      case "backtest" =>

        // このへんのパラメータもあとで取得する
        val service = new BackTestService[app.Envs.M](brains, capital, readCsv, writeCsv)
        
        signal.tryUpdate { _.run(service) }
          .>>= { isOk =>
           if isOk then Ok("BackTest Start") 
           else BadRequest("Update cannot")
          }
        
      case "demotrade" =>
        // このへんのパラメータもあとで取得する
        val wsFactory = new WebSocketFactory(Demo(brains, capital), java.time.Duration.ofMillis(1L))
        
        wsFactory
          .get("bitflyer_btc")
          .flatMap { ws => 
            signal
              .tryUpdate { _.run(new DemoTradeService(ws)) }
          }
          .>>= { isOk =>
           if isOk then Ok("DemoTrade Start")
           else BadRequest("Update cannot")
          }

      case other => BadRequest()
  }

  val stopService = HttpRoutes.of[IO] { 
    case GET -> Root / "stop"  =>
      /* トレード終了 */
      signal.update { _.idle } >> Ok(s"Stop") 
  }
