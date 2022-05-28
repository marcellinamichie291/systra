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

class BitFlyerWS(apiKey:    String,
                 apiSecret: String,
                 channel:   Channel,
                 period:    TemporalAmount) extends WebSocketStream, LazyLogging:

  override val configUri: ConfigValue[Effect, Uri] = WebSocketUrl.BITFLYER_WS_URL
  override val configMaxConnectCount: ConfigValue[Effect, Int] = default(5) // 変更するならここで変える

  override def toChart(websocket: Stream[IO, WebSocketFrame.Data[?]]): Stream[IO, (WebSocketFrame, Option[Chart])] = 
    lazy val stream = websocket
      .through(dataToResponse) // circeによるJSONのparse
      .through(responseToState) // parseしたものをさらに整形
      .flatMap {
        case ConnectionState(true, false, tickers) => Stream.emit((BFUtils.subscribeText(channel), tickers))
        case _                                     => Stream.empty
      }
      .flatMap { case (frame, tickers) =>
        Stream.emits(tickers.sortBy(_.exec_date).map((frame, _)))
      }
      .through(tickerToChart)
      
    Stream
      .emit((BFUtils.authText(apiKey, apiSecret), None))
      .append(stream)

  /* tickerをchartに変換する */
  private def tickerToChart(stream: Stream[IO, (WebSocketFrame, ExecutionInfo)]): Stream[IO, (WebSocketFrame, Option[Chart])] = stream
    .head
    .flatMap { case (frame, ExecutionInfo(_,_,price,size,exec_date,_,_)) =>
      val firstChart = Chart(price, price, price, price, size, BFUtils.parseLocalDatetime(exec_date))
      stream.through(createChart(period, (frame, firstChart)))
    }
    
  /** TickerからChartに成形する */
  private def createChart(period: TemporalAmount,
                          head:   (WebSocketFrame, Chart)): Pipe[IO, (WebSocketFrame, ExecutionInfo), (WebSocketFrame, Option[Chart])] = _
    .scan(head.asLeft[(WebSocketFrame, Chart, ExecutionInfo)]) { case (either, (_, ticker)) => 
      val execDate = BFUtils.parseLocalDatetime(ticker.exec_date)
      either match
      // Leftであれば更新していく
        case Left(frame, Chart(open, high, low, close, volume, datetime)) =>
          if (execDate compareTo datetime.plus(period)) < 0 then // 規定時間内の場合
            (frame, Chart(open, high max ticker.price, low min ticker.price, ticker.price, volume + ticker.size, datetime)).asLeft[(WebSocketFrame, Chart, ExecutionInfo)]
          else
            (frame, Chart(open, high, low, close, volume, datetime), ticker).asRight[(WebSocketFrame, Chart)] 

        // Rightであればリセットする
        case Right(frame, _, ExecutionInfo(_,_,prevPrice,prevSize,prevExecDate,_,_)) =>
          val prevDatetime = BFUtils.parseLocalDatetime(prevExecDate)
          (frame, Chart(prevPrice, ticker.price max prevPrice, ticker.price min prevPrice, ticker.price, ticker.size+prevSize, prevDatetime)).asLeft[(WebSocketFrame, Chart, ExecutionInfo)]
    }
    .map { 
      case Left(frame, _) => (frame, None)
      case Right((frame, chart, _)) => (frame, Some(chart)) }


      
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