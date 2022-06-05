package app

import app.Envs._
import app.backend._
import app.model._
import app.model.service._

import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.entity.UnixTimeStamp._

import cats._
import cats.implicits.{catsSyntaxFlatMapOps, catsSyntaxSemigroup}
import cats.effect._

import fs2._
import fs2.timeseries.{TimeStamped, TimeSeries}
import fs2.concurrent.{Topic, SignallingRef}
import fs2.concurrent.Topic.Closed

import com.comcast.ip4s._

import scala.concurrent.duration.{DurationInt, FiniteDuration} 

import java.util.Locale
import java.io.{File, FileWriter, FileInputStream}
import collection.JavaConverters._

object Playground extends IOApp:

  override def run(args: List[String]): IO[ExitCode] = test
    .as(ExitCode.Success)  

  def test = Stream
    .fixedDelay[IO](5.second)
    .zipRight(Stream.range(1, 10).repeat)
    //.map(_=> System.currentTimeMillis)
    .through(aggregateTicker(5.second))
    .evalMap { case TimeStamped(time, data) =>
      IO.println(s"Time: ${time.toMillis.toTimeStamp.show()}, data: ${data}") 
    }
    .compile
    .drain

  def aggregateTicker[N: Semigroup](period: FiniteDuration)(input: Stream[IO, N]) =
    val aggregateInterval = TimeStamped
      .rate[Option[N], Seq[N]](period)(Vector.from)
      .toPipe[IO]

    val ticker = Stream
      .fixedDelay[IO](period)
      .evalMap(_ => TimeStamped.now[IO, Option[N]](None))

    val stream = input
      .evalMap(TimeStamped.now[IO, N])
      .map(tsa => tsa.map(Some(_): Option[N]))
      .merge(ticker)
      .through(aggregateInterval)
      .dropWhile(_.value.isEmpty)

    stream
      .head
      .flatMap { first => stream
        .evalScan(first) {
          case (prev, TimeStamped(_, Nil)) => TimeStamped.now[IO, Seq[N]](prev.value)
          case (_, stamped)                => IO(stamped)
        }
        .map { ts => ts.map(_.reduceLeft(_|+|_)) }
      }

    
  def retryTest = Stream[IO, Int](1, 2, 3, 4)
    .append(Stream.raiseError[IO](new RuntimeException("This is error")))
    .attempts(Stream.constant[IO, FiniteDuration](5.second))
    .zipWithScan1(0) {
      case (connectCnt, Left(_))  => connectCnt + 1
      case (connectCnt, Right(_)) => connectCnt
    }
    .takeWhile { case (_, connectCount) => connectCount < 5 }
    .flatMap {
      case (Left(t), idx) => 
        println(s"Exception occured. ${t.getMessage}")
        println(s"WebSocket reconnect count: $idx/5")
        Stream.empty
      case (Right(num), _) => Stream.emit(num)
    }
    .scan(0)(_+_)
    .map { num =>
      println(s"printed $num")
      num
    }
    .onComplete { 
      println("Websocket is finished.")
      Stream.empty
    }
    .compile
    .drain
    .as(ExitCode.Success)
    
  def runStatusTest = for
    status <- SignallingRef[IO, AppStatus[Service]](Idle)
    _      <- IO.asyncForIO.start(useServer(status))
  //  _      <- IO.asyncForIO.foreverM(status.get >>= { s => IO.whenA(!s.isIdled) { IO.println(s) >> status.set(Idle) } })
    _      <- IO.asyncForIO.foreverM(status.get >>= { s => IO.println(s) })
  yield ExitCode.Success

  def useServer(statusRef: StatusRef[Service]): IO[Unit] = for
    host   <- LOCAL_HOST.load[IO]
    port   <- PORT.load[IO]
    ?      <- HttpBackend.getServer(host, port)(statusRef).useForever
  yield ()