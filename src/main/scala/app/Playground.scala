package app

import cats.implicits._
import cats.effect._

import fs2._
import fs2.concurrent.Topic
import fs2.concurrent.SignallingRef
import fs2.concurrent.Topic.Closed

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

object Playground extends IOApp:
  override def run(args: List[String]): IO[ExitCode] = 
    val root = Map[String, String]("user" -> "大")
    freemarkerProcess("greeting.ftl", root).as(ExitCode.Success)
    
  def freemarkerProcess(template: String, data: Map[String, String]) = IO {
    import java.io.File
    import java.io.OutputStreamWriter
    import java.io.Writer
    import freemarker.template.{Configuration, Template}
    import collection.JavaConverters._

    val cfg = new Configuration()
    cfg.setDirectoryForTemplateLoading(new File("""src\main\resources\template"""))
    val out: Writer = new OutputStreamWriter(System.out)
    val temp: Template = cfg.getTemplate(template)
    temp.process(data.asJava, out)
    out.flush()
  }

  /*for 
    ?   <- IO.println("start")
    signal <- SignallingRef[IO, Boolean](false)
    ?   <- stream.compile.drain
    ?   <- IO.println("end")
  yield
    ExitCode.Success

  def stream: Stream[IO, Unit] = for 
    topic   <- Stream.eval(Topic[IO, Event])
    signal  <- Stream.eval(SignallingRef[IO, Boolean](false))
    _       <- service(topic, signal).start(List("A", "B", "C", "D"))
  yield ()
  */

  def service(topic: Topic[IO, Event], haltWhenTrue: SignallingRef[IO, Boolean]) = new EventService(topic, haltWhenTrue) {
    override def publisher: Stream[IO, Either[Closed, Unit]] = stdinStream
      .evalMap {
        case ":s" => topic.publish1(Start)
        case ":q" => topic.publish1(Stop)
        case ":k" => topic.publish1(Kill)
        case ":i" => topic.publish1(Interrupt)
        case str  => topic.publish1(Start)
      }
      .interruptWhen(haltWhenTrue)
      
    
    override def subscriber(id: String): Stream[IO, Unit] = topic
      .subscribe(maxQueued = 1) flatMap {
        case Start     => printlnStream(s"#$id GET Message")
        case Stop      => printlnStream(s"#$id STOP") ++ Stream.eval(haltWhenTrue.set(true))
        case Kill      => printlnStream(s"#$id KILL") ++ Stream.eval(haltWhenTrue.set(true))
        case Interrupt => printlnStream(s"#$id INTERRUPT") ++ Stream.eval(haltWhenTrue.set(true))
      }
  }

  def stdinStream = io
    .stdin[IO](4096)
    .through(text.utf8.decode)
    .through(text.lines)
    .map(_.trim)
    .filter(_.nonEmpty)
    .takeWhile(_ != ":k", takeFailure = true) // ":q" まで読み込む


  def printlnStream(line: String): Stream[IO, Unit] = Stream(s"$line\n")
    .through(text.utf8.encode)
    .through(io.stdout)