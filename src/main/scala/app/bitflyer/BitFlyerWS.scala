package app.bitflyer

import app.WebSocketUrl
import app.bitflyer.BFReq._
import app.bitflyer.BFRes._
import app.bitflyer.BFUtils.{given_Initial_ConnectionState ,ConnectionState}

import com.github.imomushi8.systra.chart._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.util._

import cats.implicits._
import cats.effect._
import fs2.{Stream, Pipe}

import io.circe.{Encoder, Decoder, DecodingFailure}
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._

import ciris._

import sttp.client3._
import sttp.model.Uri
import sttp.ws.{WebSocket, WebSocketFrame}

import com.typesafe.scalalogging.LazyLogging
import java.time.temporal.TemporalAmount

class BitFlyerWS(executePipe: Chart => Pipe[IO, Chart, Unit],
                 apiKey:      String,
                 apiSecret:   String,
                 channel:     Channel,
                 period:      TemporalAmount) extends WebSocketStream, LazyLogging:

  /** トレードの実行 */
  def executeTrade(chartStream: Stream[IO, Chart]): Stream[IO, WebSocketFrame] = chartStream
    .head
    .flatMap { head => chartStream
      .through(executePipe(head))
      .drain
    }
  
  override val configUri: ConfigValue[Effect, Uri] = WebSocketUrl.BITFLYER_WS_URL

  override def callback(websocket: Stream[IO, WebSocketFrame.Data[?]]): Stream[IO, WebSocketFrame] = 
    val stream = websocket
      .through(dataToResponse) // circeによるJSONのparse
      .through(responseToState) // parseしたものをさらに整形
      .broadcastThrough(executionPipe, replyPipe) // websocketに返信するPipeとトレード実行するPipeにデータを渡す
      .handleErrorWith { throwable => 
        logger.error(s"Exception occured. ${throwable.getMessage}")
        Stream.emit(WebSocketFrame.close)
        /* TODO: handleErrorWithの中身にIORefを更新する仕組みを作って（もしくはscanを使って状態更新して）、
         *       IORefの中身が規定回数以上になったらWebSocketFrame.closeを出力、それ以外は再接続（←これむずそう）
         */
      }

    Stream.emit(BFUtils.authText(apiKey, apiSecret)).append(stream)
    
  private val dataToResponse: Pipe[IO, WebSocketFrame.Data[?], Either[io.circe.Error, JsonRpc]] = _
    .map {
      case WebSocketFrame.Text(payload, _, _) =>
        logger.debug(s"GET for Websocket: $payload")
        parse(payload).flatMap { json => json
          .as[JsonRpcRes[Boolean]]
          .leftFlatMap{ _=>json.as[JsonRpcReq[BFRes.Execution]] }
        } 
      case frame => throw new RuntimeException(s"WebSocketFrame is invalid. frame: $frame")
    }

  private val responseToState: Pipe[IO, Either[io.circe.Error, JsonRpc], ConnectionState] = _
    .scan(Initial[ConnectionState]()) { case (state, eitherRes) => eitherRes match
      /* 認証結果の場合 */
      case Right(JsonRpcRes(_, 1, Some(true), None)) =>
        logger.info("BitFlyer Auth is Succeeded")
        state.copy(passAuth=true)

      /* チャンネル購読結果の場合 */
      case Right(JsonRpcRes(_, 2, Some(true), None)) =>
        logger.info(s"Channel '$channel' subscribe is Succeeded")
        state.copy(isSubscribed=true)

      /* チャンネル購読後のデータ */
      case Right(JsonRpcReq(_, _, Execution(_, message), _)) => state.copy(tickers=message)

      /* RequestError */
      case Right(JsonRpcRes(_, _, None, Some(error))) => throw new RuntimeException(s"Request Error: ${error.asJson.noSpaces}")
      /* どれにも当てはまらない場合 */
      case Right(res) => throw new RuntimeException(s"Pattern match Exception: response: $res")
      /* ParseError */
      case Left(failure) => throw failure
    }

  /** websocketから受け取るデータをコールバック関数のように回していくPipe */
  private val replyPipe: Pipe[IO, ConnectionState, WebSocketFrame] = _
    .flatMap {
      case ConnectionState( true, false, _) => Stream.emit(BFUtils.subscribeText(channel))
      case _                                => Stream.empty
    }

  /** アルゴリズム取引を執行するPipe */
  private val executionPipe: Pipe[IO, ConnectionState, WebSocketFrame] = stream => 
    val executionStream = stream
      .map { case ConnectionState(_, _, tickers) => tickers.sortBy(_.exec_date) }
      .flatMap(Stream.emits)

    executionStream
      .head
      .flatMap { case ExecutionInfo(_,_,price,size,exec_date,_,_) => 
        val firstChart = Chart(price, price, price, price, size, BFUtils.parseLocalDatetime(exec_date))
        executionStream.through(createChart(firstChart)) 
      }
      .through(executeTrade)

  /** TickerからChartに成形する */
  private def createChart(head: Chart): Pipe[IO, ExecutionInfo, Chart] = _
    .scan(head.asLeft[(Chart, ExecutionInfo)]) { (either, ticker) => 
      val execDate = BFUtils.parseLocalDatetime(ticker.exec_date)
      either match
      // Leftであれば更新していく
        case Left(Chart(open, high, low, close, volume, datetime)) =>
          if (execDate compareTo datetime.plus(period)) < 0 then // 規定時間内の場合
            Chart(open, high max ticker.price, low min ticker.price, ticker.price, volume + ticker.size, datetime).asLeft[(Chart, ExecutionInfo)]
          else
            (Chart(open, high, low, close, volume, datetime), ticker).asRight[Chart] 

        // Rightであればリセットする
        case Right(_, ExecutionInfo(_,_,prevPrice,prevSize,prevExecDate,_,_)) =>
          val prevDatetime = BFUtils.parseLocalDatetime(prevExecDate)
          Chart(prevPrice, ticker.price max prevPrice, ticker.price min prevPrice, ticker.price, ticker.size+prevSize, prevDatetime).asLeft[(Chart, ExecutionInfo)]
    }
    .collect { case Right((chart, _)) => chart }
