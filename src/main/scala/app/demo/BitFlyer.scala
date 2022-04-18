package app.demo

import app.demo._
import app.demo.BitFlyerRes.ExecutionInfo

import com.github.imomushi8.systra.demo.Demo
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action.Brain
import com.github.imomushi8.systra.core.data.ChartStream
import com.github.imomushi8.systra.core.market.{Tradable, OK, TradeState}
import com.github.imomushi8.systra.backtest.BTMarket

import cats.implicits._
import cats.effect._
import fs2.{Stream, Pipe}

import io.circe.{Encoder, Decoder, DecodingFailure}
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._

import sttp.model.Uri
import sttp.client3._
import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend
import sttp.capabilities.fs2.Fs2Streams
import sttp.ws.{WebSocket, WebSocketFrame}

import com.typesafe.scalalogging.LazyLogging

case class ConnectionState(passAuth: Boolean, 
                           isSubscribed: Boolean,
                           isError: Boolean, 
                           tickers: Seq[ExecutionInfo],
                           chartOp: Option[Chart])

given Initial[ConnectionState] = new Initial[ConnectionState] { 
  def empty(): ConnectionState = ConnectionState(false,false,false, Nil, None)
}

class BitFlyer[Memory: Initial](brains:         Seq[(String, Brain[BTMarket, Memory])],
                                firstCapital:   Price,
                                apiKey:         String,
                                apiSecret:      String,
                                publicChannel:  Channel) extends Demo(brains, firstCapital, uri"wss://ws.lightstream.bitflyer.com/json-rpc"), LazyLogging:
  
  def wsPipe: Pipe[IO, WebSocketFrame.Data[?], WebSocketFrame] = input =>
    val websocket = input
      .through(dataToResponse)
      .through(responseToState)
      .through(stateToWsFrame)
      
    Stream.emit(BFUtils.authText(apiKey, apiSecret)).append(websocket) // 認証用のStreamを流した後に上記をコールバック関数のように回していく

  

  private def dataToResponse: Pipe[IO, WebSocketFrame.Data[?], Either[io.circe.Error, BitFlyerRes]] = _
    .map {
      case WebSocketFrame.Text(payload, _, _) =>
        logger.debug(s"GET for Websocket: $payload")
        parse(payload).flatMap { json => json
          .as[BitFlyerRes.Result]
          .leftFlatMap{_=>json.as[BitFlyerRes.Error]}
          .leftFlatMap{_=>json.as[BitFlyerRes.Executions]}
        } 
      case _ => DecodingFailure("WebSocketFrame is invalid.", Nil).asLeft[BitFlyerRes]
    }

  private def responseToState: Pipe[IO, Either[io.circe.Error, BitFlyerRes], ConnectionState] = _
    .scan(Initial[ConnectionState]()) { case (state, eitherRes) => eitherRes match
        /* 認証後の場合 */
        case Right(BitFlyerRes.Result(_, 1, true)) =>
          logger.info("Auth is Succeeded")
          state.copy(passAuth=true)

        /* チャンネル購読後の場合 */
        case Right(BitFlyerRes.Result(_, 2, true)) =>
          logger.info("Channel subscribe is Succeeded")
          state.copy(isSubscribed=true)

        /* チャンネル購読した場合に届くもの（約定履歴） */
        case Right(BitFlyerRes.Executions(_, _, params)) =>
          // TODO: ここにExecutionInfo => Chartの処理を記述するといい感じかも
          /*
          val stream = Stream.emits(tickers).through(createChart(ExecutionInfo(0L,"",0,0,"","","")))
          stream
            .head
            .flatMap { head => stream
              .broadcastThrough(summerize[Memory](brains, firstCapital, head)*)
              .map(_.toString)
            }
            .>> { Stream.empty }
          */
          state.copy(tickers=params.message)

        /* RequestError */
        case Right(BitFlyerRes.Error(_, id, error)) =>
          logger.error(s"Request Error: code: ${error.code}, message: ${error.message}")
          state.copy(isError=true)

        /* どれにも当てはまらない場合 */
        case Right(res) => 
          logger.error(s"Another pattern: response: $res")
          state.copy(isError=true)

        /* ParseError */
        case Left(failure) =>
          logger.error(s"Parse Error: message: $failure")
          state.copy(isError=true)
      }

  private val stateToWsFrame: Pipe[IO, ConnectionState, WebSocketFrame] = _
    .flatMap {
      case ConnectionState( true, false, false,       _, _) => Stream.emit(BFUtils.subscribeText(publicChannel))
      case ConnectionState( true,  true, false, tickers, _) => Stream.empty
      case ConnectionState(false,     _, false,       _, _) => Stream.empty
      case ConnectionState(    _,     _,  true,       _, _) => Stream.emit(WebSocketFrame.close)
    }

  // TODO: 実装
  private def createChart(head: ExecutionInfo): Pipe[IO, ExecutionInfo, Chart] = ???