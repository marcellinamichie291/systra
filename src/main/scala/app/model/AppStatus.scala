package app.model

import cats.Functor
import org.http4s.client.defaults

type RecoverCount = Int

sealed trait AppStatus[+S]
case object Idle                                          extends AppStatus[Nothing]
case class Run[S](service: S, recoverCount: RecoverCount) extends AppStatus[S]

object AppStatus:
  def _run[S](s: S)(now: AppStatus[S]): AppStatus[S] = now match
    case Idle  => Run(s, 1)
    case other => other

  def _reRun[S](now: AppStatus[S]): AppStatus[S] = now match
    case Run(s, c) => Run(s, c + 1)
    case other     => other

  def _idle[S]: AppStatus[S] = Idle

  def _isIdled[S](now: AppStatus[S]): Boolean = now match
    case Idle => true
    case _    => false

  def _getOrElse[S](default: S)(now: AppStatus[S]): S = now match
    case Idle            => default
    case Run(service, _) => service

  given Functor[AppStatus] with {
    def map[A, B](fa: AppStatus[A])(f: A => B): AppStatus[B] = fa match
      case Idle      => Idle
      case Run(a, c) => Run(f(a), c)
  }

  extension [S](now: AppStatus[S]) {
    def run(s: S): AppStatus[S]  = _run(s)(now)
    def reRun: AppStatus[S]  = _reRun(now)
    def idle: AppStatus[S]       = _idle
    def isIdled: Boolean         = _isIdled(now)
    def getOrElse(default: S): S = _getOrElse(default)(now)
  }
