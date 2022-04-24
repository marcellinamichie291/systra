package app

import cats.implicits._
import cats.effect._

import fs2._
import fs2.concurrent.{Topic, SignallingRef}

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

sealed trait Event
case object Start extends Event
case object Stop extends Event
case object Kill extends Event
case object Interrupt extends Event

trait EventService(topic: Topic[IO, Event], haltWhenTrue: SignallingRef[IO, Boolean]):
  def publisher: Stream[IO, Either[Topic.Closed, Unit]]
  def subscriber(id: String): Stream[IO, Unit]

  def start(subscribersName: Seq[String]): Stream[IO, INothing] =
    val subscribers: Stream[IO, Unit] = Stream.emits(subscribersName)
      .map(subscriber)
      .parJoin(subscribersName.length)
    
    Stream(publisher concurrently subscribers)
      .parJoin(subscribersName.length)
      .drain