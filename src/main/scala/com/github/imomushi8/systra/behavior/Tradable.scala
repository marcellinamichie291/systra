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
import org.atnos.eff.syntax.all._
import org.atnos.eff.Members.extractMember
import org.atnos.eff.state._
import org.atnos.eff.writer._
import cats.Eval

trait Tradable[Market](using MarketBehavior[Market]) 
  extends EitherEffect with EvalEffect with StateEffect with WriterEffect:

  type _tState[Memory] = [R] =>> State[(Market, Memory), *] |= R
  type _tWriter = [R] =>> Writer[Vector[Report], *] |= R
  
  type Stack[Memory] = Fx.fx3[State[(Market, Memory), *], Writer[Vector[Report], *], Either[Throwable, *]]
  
  def trade[Memory](brain: Brain[Market, Memory])
                   (chart: Chart): Eff[Stack[Memory], Unit] = 
    tradeFlow[Memory, Stack[Memory]](brain)(chart)

  def tradeFlow[Memory, R: _throwableEither: _tState[Memory]: _tWriter]
    (brain: Brain[Market, Memory])(chart: Chart): Eff[R, Unit] = 
      for
        ?       <- updateChart(chart)
        reports <- checkContract[Memory, R]
        ?       <- tell(reports)
        action  <- execAction(chart, brain)
        ?       <- action match
                    case Next(market, memory) => put((market, memory))
                    case End(t)               => left(t)
        next    <- get
      yield ()
    
  def run[Memory](effect: Eff[Stack[Memory], Unit], current: (Market, Memory)) =
    effect.runState(current).runWriterMonoid.runEither.run

  def checkContract[Memory, R: _throwableEither: _tState[Memory]: _tWriter]: Eff[R, Vector[Report]] =
    def runContract(current: (Market, Memory)): IO[Eff[R, Vector[Report]]] = current match
      case (market, memory) => market.contract map {
        case (nextMarket, logs) => put((nextMarket, memory)) *> Eff.pure(logs.toVector)
      } handleError {t => left(t)}

    get >>= (runContract(_).unsafeRunSync())

  def updateChart[Memory, R: _throwableEither: _tState[Memory]: _tWriter](chart: Chart): Eff[R, Unit] =
    modify[R, (Market, Memory)] { 
      case (market, memory) => (market := chart, memory)
    }

  def execAction[Memory, R: _throwableEither: _tState[Memory]: _tWriter](chart: Chart, brain: Brain[Market, Memory]): Eff[R, TradeAction[Market, Memory]] = 
    gets[R, (Market, Memory), TradeAction[Market, Memory]] { 
      case (market, memory) => brain(chart, market.context, memory)
    }