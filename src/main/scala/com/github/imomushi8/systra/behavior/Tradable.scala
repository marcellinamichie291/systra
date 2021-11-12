package com.github.imomushi8.systra.behavior

import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.{MarketContext, TradeAction, Next, End}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.behavior._

import cats.data._
import cats.syntax.flatMap.catsSyntaxFlatMapOps
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.unsafe.implicits.global

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.state._
import org.atnos.eff.ErrorEffect._
import org.atnos.eff.writer._

trait Tradable[Market](using MarketBehavior[Market]) 
  extends ErrorEffect[Throwable] with StateEffect with WriterEffect:

  type _tState[Memory] = [R] =>> State[(Market, Memory), _] |= R
  type _tWriter = [R] =>> Writer[Vector[Report], _] |= R
  
  def checkContract[Memory, R: _errorOrOk: _tState[Memory]: _tWriter]: Eff[R, Vector[Report]] =
    def runContract(current: (Market, Memory)): IO[Eff[R, Vector[Report]]] = current match
      case (market, memory) => market.contract map {
        case (nextMarket, logs) => put((nextMarket, memory)) *> Eff.pure(logs.toVector)
      } handleError (fail)

    get >>= (runContract(_).unsafeRunSync())

  def updateChart[Memory, R: _errorOrOk: _tState[Memory]: _tWriter](chart: Chart): Eff[R, Unit] =
    modify[R, (Market, Memory)] { 
      case (market, memory) => (market := chart, memory)
    }

  def execAction[Memory, R: _errorOrOk: _tState[Memory]: _tWriter](chart: Chart, brain: Brain[Market, Memory]): Eff[R, TradeAction[Market, Memory]] = 
    gets[R, (Market, Memory), TradeAction[Market, Memory]] { 
      case (market, memory) => brain(chart, market.context, memory)
    }
  
  def trade[Memory, R: _errorOrOk: _tState[Memory]: _tWriter](brain: Brain[Market, Memory])(chart: Chart): Eff[R, Unit] = 
    for
      ?       <- updateChart(chart)
      reports <- checkContract[Memory, R]
      ?       <- tell(reports)
      action  <- execAction(chart, brain)
      ?       <- action match
                  case Next(market, memory) => put((market, memory))
                  case End(t)               => fail(t)
    yield ()