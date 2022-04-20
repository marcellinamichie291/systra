package app.bitflyer

import app.Envs
import app.bitflyer.BitFlyerRes.ExecutionInfo
import app.bitflyer.BFUtils.{given_Initial_ConnectionState ,ConnectionState}

import com.github.imomushi8.systra.core.data._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.util.Initial

import cats.implicits._
import cats.effect._
import fs2.{Stream, Pipe}

import io.circe.{Encoder, Decoder, DecodingFailure}
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._

import sttp.client3._
import sttp.model.Uri
import sttp.ws.{WebSocket, WebSocketFrame}

import com.typesafe.scalalogging.LazyLogging


trait BFStream(apiKey:    String,
               apiSecret: String,
               channel:   Channel) extends WebSocketStream, LazyLogging:
  def executeTrade(chartStream: Stream[IO, Chart]): Stream[IO, WebSocketFrame]

  val uri: Uri = Envs.BITFLYER_WS_URL

  override def callback(websocket: Stream[IO, WebSocketFrame.Data[?]]): Stream[IO, WebSocketFrame] = Stream
    .emit(BFUtils.authText(apiKey, apiSecret))
    .append(websocket
      .through(dataToResponse) // circeによるJSONのparse
      .through(responseToState) // parseしたものをさらに整形
      .broadcastThrough(executionPipe, replyPipe) // websocketに返信するPipeとトレード実行するPipeにデータを渡す
    )
    
  private val dataToResponse: Pipe[IO, WebSocketFrame.Data[?], Either[io.circe.Error, BitFlyerRes]] = _
    .map {
      case WebSocketFrame.Text(payload, _, _) =>
        logger.debug(s"GET for Websocket: $payload")
        parse(payload).flatMap { json => json
          .as[BitFlyerRes.Result]
          .leftFlatMap{_=>json.as[BitFlyerRes.Error]}
          .leftFlatMap{_=>json.as[BitFlyerRes.Executions]}
        } 
      case frame => throw new Exception(s"WebSocketFrame is invalid. frame: $frame")
    }

  private val responseToState: Pipe[IO, Either[io.circe.Error, BitFlyerRes], ConnectionState] = _
    .scan(Initial[ConnectionState]()) { case (state, eitherRes) => eitherRes match
      /* 認証結果の場合 */
      case Right(BitFlyerRes.Result(_, 1, true)) =>
        logger.info("Auth is Succeeded")
        state.copy(passAuth=true)

      /* チャンネル購読結果の場合 */
      case Right(BitFlyerRes.Result(_, 2, true)) =>
        logger.info("Channel subscribe is Succeeded")
        state.copy(isSubscribed=true)

      /* チャンネル購読後のデータ */
      case Right(BitFlyerRes.Executions(_, _, params)) => state.copy(tickers=params.message)

      /* RequestError */
      case Right(BitFlyerRes.Error(_, id, error)) => throw Exception(s"Request Error: ${error.asJson.noSpaces}")
      /* どれにも当てはまらない場合 */
      case Right(res) => throw Exception(s"Pattern match Exception: response: $res")
      /* ParseError */
      case Left(failure) => throw Exception(s"Parse Error: ${failure.getMessage}")
     }


  /** websocketから受け取るデータをコールバック関数のように回していくPipe */
  private val replyPipe: Pipe[IO, ConnectionState, WebSocketFrame] = _
    .flatMap {
      case ConnectionState( true, false, _) => Stream.emit(BFUtils.subscribeText(channel))
      case ConnectionState( true,  true, _) => Stream.empty
      case ConnectionState(false,     _, _) => Stream.empty
    }
    .handleErrorWith { throwable => 
      logger.error(s"Exception occured. ${throwable.getMessage}")
      Stream.emit(WebSocketFrame.close) 
    }

  /** アルゴリズム取引を執行するPipe */
  private val executionPipe: Pipe[IO, ConnectionState, WebSocketFrame] = stream => 
    val executionStream = stream
      .map { case ConnectionState(_, _, tickers) => tickers.sortBy(_.exec_date) }
      .handleError { _ => Nil }
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
          if (execDate compareTo datetime.plusMinutes(1)) < 0 then // 規定時間内の場合
            Chart(open, high max ticker.price, low min ticker.price, ticker.price, volume + ticker.size, datetime).asLeft[(Chart, ExecutionInfo)]
          else
            (Chart(open, high, low, close, volume, datetime), ticker).asRight[Chart] 

        // Rightであればリセットする
        case Right(_, ExecutionInfo(_,_,prevPrice,prevSize,prevExecDate,_,_)) =>
          val prevDatetime = BFUtils.parseLocalDatetime(prevExecDate)
          Chart(prevPrice, ticker.price max prevPrice, ticker.price min prevPrice, ticker.price, ticker.size+prevSize, prevDatetime).asLeft[(Chart, ExecutionInfo)]
    }
    .collect { case Right((chart, _)) => chart }