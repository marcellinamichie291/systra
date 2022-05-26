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

/**
 * ルーティング
 */
object Routes:

  val brains   = app.Envs.brains
  val capital  = app.Envs.leveragedCapital
  val readCsv  = app.Envs.readCsvPath
  val writeCsv = app.Envs.writeCsvPath

  object QueryMarket extends QueryParamDecoderMatcher[String]("market")
  object QueryPeriod extends QueryParamDecoderMatcher[String]("period")

  /**
   * ルーティング
   */
  def apply(signal: SignallingRef[IO, AppStatus[Service]])(
      using clock: Clock[IO]) = HttpRoutes.of[IO] {

    /* バックテスト開始 */
    case GET -> Root / "start" / "backtest" =>

      // このへんのパラメータもあとで取得する
      val service = new BackTestService[app.Envs.M](brains, capital, readCsv, writeCsv)
      
      for
        isIdled <- signal.get.map(_.isIdled)
        isOk    <- if isIdled then signal.tryUpdate { _.run(service) } else IO(false)
        res     <- if isOk then Ok("BackTest Start") else BadRequest("Update cannot")
      yield res

    /* デモ取引開始 */
    case GET -> Root / "start" / "demotrade" =>
      
      // このへんのパラメータもあとで取得する
      val wsFactory = new WebSocketFactory(Demo(brains, capital), java.time.Duration.ofMillis(1L))
      
      for
        ws      <- wsFactory.get("bitflyer_btc")
        isIdled <- signal.get.map(_.isIdled)
        isOk    <- if isIdled then signal.tryUpdate { _.run(new DemoTradeService(ws)) } else IO(false)
        res     <-
          if isOk then Ok("DemoTrade Start") else BadRequest("Update cannot")
      yield res

    /* トレード終了 */
    case GET -> Root / "stop" =>
      signal.update { _.idle } >> Ok(s"Stop")
  }
