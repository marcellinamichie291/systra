package com.github.imomushi8.systra.behavior

import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.{MarketContext, TradeAction, Next, End}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.behavior._

import cats.data._
import cats.implicits._
import cats.syntax.flatMap.catsSyntaxFlatMapOps
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.unsafe.implicits.global

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.state._
import org.atnos.eff.writer._

import fs2._

trait Tradable[Market](using MarketBehavior[Market]) 
  extends EitherEffect with EvalEffect with StateEffect:// with WriterEffect:

  type _tState[Memory] = [R] =>> State[(Market, Memory), *] |= R

  type Stack[Memory] = Fx.fx2[State[(Market, Memory), *], Either[Throwable, *]]

  /* レポート出るたびにStreamに出力してほしいので、ログ保存する意味ない（一応コメントアウト） */
  //type _tWriter = [R] =>> Writer[Vector[Report], *] |= R
  //type Stack[Memory] = Fx.fx3[State[(Market, Memory), *], Writer[Vector[Report], *], Either[Throwable, *]]
  
  def apply[Memory, F[_]](brain: Brain[Market, Memory])(init: (Market, Memory)): Pipe[F, Chart, Either[Throwable, (Vector[Report], (Market, Memory))]] =
    _.scan((Vector.empty[Report], init).asRight[Throwable]) { case (current, chart) => current match
      case Right((_, state)) => run(trade(brain)(chart), state)
      case Left(throwable) => throw throwable
    }

  def trade[Memory](brain: Brain[Market, Memory]): Chart => Eff[Stack[Memory], Vector[Report]] = 
    tradeFlow[Memory, Stack[Memory]](brain)

  def tradeFlow[Memory, R: _throwableEither: _tState[Memory]]
    (brain: Brain[Market, Memory])(chart: Chart): Eff[R, Vector[Report]] = 
      for
        ?       <- updateChart(chart)
        reports <- checkContract[Memory, R]
        action  <- execAction(chart, brain)
        ?       <- action match
                    case Next(market, memory) => put((market, memory))
                    case End(endMsg)               => left(new Throwable(endMsg))
      yield reports
    
  def run[A, Memory](effect: Eff[Stack[Memory], A], init: (Market, Memory)): Either[Throwable, (A, (Market, Memory))] =
    effect.runState(init).runEither.run

  def checkContract[Memory, R: _throwableEither: _tState[Memory]]: Eff[R, Vector[Report]] =
    def runContract(current: (Market, Memory)): IO[Eff[R, Vector[Report]]] = current match
      case (market, memory) => market.contract map {
        case (nextMarket, logs) => put((nextMarket, memory)) *> pure(logs.toVector)
      } handleError {t => left(t)}

    get >>= (runContract(_).unsafeRunSync())

  def updateChart[Memory, R: _throwableEither: _tState[Memory]](chart: Chart): Eff[R, Unit] =
    modify[R, (Market, Memory)] { 
      case (market, memory) => (market := chart, memory)
    }

  def execAction[Memory, R: _throwableEither: _tState[Memory]](chart: Chart, brain: Brain[Market, Memory]): Eff[R, TradeAction[Market, Memory]] = 
    gets[R, (Market, Memory), TradeAction[Market, Memory]] { 
      case (market, memory) => brain(chart, market.context, memory)
    }