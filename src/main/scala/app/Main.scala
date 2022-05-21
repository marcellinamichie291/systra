package app

import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity.Chart
import com.github.imomushi8.systra.virtual.VirtualMarket._

import app.Envs._
import app.backend._
import app.model._
import app.model.service._
import app.demo._
import app.bitflyer._
import app.backtest._

import cats.syntax.all._
import cats.implicits._
import cats.effect._

import fs2._
import fs2.concurrent.SignallingRef

import sttp.ws.WebSocketFrame
import com.typesafe.scalalogging.LazyLogging
import java.time.temporal.TemporalAmount
import scala.concurrent.duration.DurationInt

/*
 * TODO: レポート等の出力形式をまとめたtraitづくり（できれば）
 * TODO: WebSocket切断時に、再接続ができるようにしたい（最大接続回数をどこかに入力させて（configファイルとかに）、それにしたがって接続させる）
 */

/** 
   * シグナル管理メソッド
   * ５分後にsignalをtrueにする（trueにするとWebSocketが終了する）
   *
    for
      apiKey    <- BITFLYER_API_KEY.load[IO]
      apiSecret <- BITFLYER_API_SECRET.load[IO]
      channel   <- BITFLYER_PUBLIC_CHANNEL.load[IO]
      signal    <- SignallingRef[IO, Boolean](false)
      ws        <- IO { DemoBitFlyerWS(brains, leveragedCapital, apiKey.value, apiSecret.value, channel, period) }
      シグナル管理のIOを別スレッドで実行させる（並列実行させる）
      ? <- IO.asyncForIO.start(IO.sleep(5.minutes) >> signal.set(true))
    yield ()
*/

object Main extends LazyLogging, IOApp:
  override def run(args: List[String]): IO[ExitCode] = for
    ?      <- IO.println("start")

    status <- SignallingRef[IO, AppStatus[Service]](Idle)
    host   <- LOCAL_HOST.load[IO]
    port   <- PORT.load[IO]
    ?      <- IO.asyncForIO.start(HttpBackend.getServer(status, host, port).useForever)
    //?      <- IO.asyncForIO.foreverM(status.get >>= { s =>  IO.whenA(!s.isIdled) { IO.println(s) >> status.set(Idle)} } )
    ?      <- IO.asyncForIO.foreverM(TradeApp.start(status) >> status.set(Idle) ) // 実行中は待機していてほしい

    ?      <- IO.println("end")
  yield
    ExitCode.Success