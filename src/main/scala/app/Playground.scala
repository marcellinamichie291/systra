package app

import app.Envs._
import app.backend._
import app.model._
import app.model.service._

import cats.implicits._
import cats.effect._
import cats.effect.unsafe.implicits.global

import fs2._
import fs2.concurrent.{Topic, SignallingRef}
import fs2.concurrent.Topic.Closed
import com.comcast.ip4s._

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import collection.JavaConverters._

import java.util.Locale
import java.io.File
import java.io.FileWriter
import java.io.FileInputStream

object Playground extends IOApp:

  override def run(args: List[String]): IO[ExitCode] = test
    .as(ExitCode.Success)
  
  import fs2.{Stream, Chunk, Scan}
  import fs2.timeseries.{TimeStamped, TimeSeries}
  import cats.effect.{Ref, Temporal}
  import scodec.bits.ByteVector
  import scala.collection.immutable.Queue
  import scala.concurrent.duration._

  def test = Stream
    .fixedDelay[IO](300.milli)
    .zipRight(Stream.range(1, 10).repeat)
    .through(measureAverageBitrate)
    //.printlns
    .compile
    .drain

  def averageBitrate = TimeStamped
    .withRate[Option[Int], Seq[Int]](3.second)(_.map{ i=>Seq(i) }.getOrElse(Nil))
    /*.andThen(Scan.stateful1(Queue.empty[Int]) {
      case (q, tsv @ TimeStamped(_, Right(_))) => (q, tsv)
      case (q, TimeStamped(t, Left(seq))) =>
        println(s"QUEUE: $q")
        println(s"SEQ: $seq")
        (q, TimeStamped(t, Left((seq))))// ++ q))))//.take(20))))
    })*/

  def measureAverageBitrate[F[_]: Temporal](input: Stream[F, Int]): Stream[F, Int] =
    TimeSeries.timePulled(input, 0.second, 3.second)
      .through(averageBitrate.toPipe)
      .flatMap {
        case TimeStamped(_, Left(bitrate)) => 
          println(s"LEFT: $bitrate")
          Stream.empty
        case TimeStamped(_, Right(Some(bytes))) => 
          //println(s"RIGHT: $bytes")
          Stream.empty
        case TimeStamped(_, Right(None)) => Stream.empty
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