package com.github.imomushi8.systra.core.data

import com.github.imomushi8.systra.report._
import com.github.imomushi8.systra.core.data.ChartStream
import com.github.imomushi8.systra.core.entity.Chart

import cats.implicits._
import cats.effect._

import fs2._
import fs2.io.file.{Files, Path}

import com.github.gekomad.ittocsv.core.FromCsv.Decoder
import com.github.gekomad.ittocsv.parser.io.FromFile._
import com.github.gekomad.ittocsv.parser.IttoCSVFormat
import deriving.Mirror.ProductOf
import java.time.LocalDateTime

trait CsvStream:
  
  def summarize(chartStream: Stream[IO, Chart]): Stream[IO, SummaryReport]

  /** Csvの開始メソッド */
  def begin[Csv](csvPath:    String)
                (csvToChart: Csv => Chart)
                (using p:    ProductOf[Csv],
                       d:    Decoder[List[String], p.MirroredElemTypes],
                       i:    IttoCSVFormat): Ops = 
    val chartStream = csvFromFileStream[Csv](csvPath, skipHeader = false)
    val head = chartStream.head.map {
        case Left(line) => throw Exception()
        case Right(csv) => csvToChart(csv)
    }
    
    val stream = head.flatMap { headChart =>
      chartStream
      .scan(headChart) { (prev, csvEither) => csvEither match 
        case Left(_)    => prev
        case Right(csv) => csvToChart(csv)
      }
    }

    Ops(stream)

  class Ops(stream: Stream[IO, Chart]):
    /** ダウンサンプリング */
    def downSampling(): Ops = new Ops(stream
      .head
      .flatMap { head => stream
        .scan(head.asLeft[(Chart, Chart)]) {
          // Leftであれば更新していく
          case (Left(Chart(open, high, low, close, volume, datetime)), chart) =>
            if (chart.datetime compareTo datetime.plusMinutes(5)) < 0 then // 規定時間内の場合
              Chart(open, high max chart.high, low min chart.low, chart.close, volume+chart.volume, datetime).asLeft[(Chart, Chart)]
            else
              (Chart(open, high, low, close, volume, datetime), chart).asRight[Chart] 

          // Rightであればリセットする
          case (Right(_, prev), Chart(_, high, low, close, volume, _)) =>
            Chart(prev.open, high max prev.high, low min prev.low, close, volume+prev.volume, prev.datetime).asLeft[(Chart, Chart)]
        }
        .collect { case (Right((chart, _))) => chart }
    })

    /** BackTestの終了メソッド */
    def end(writeCsvPath: String): IO[Unit] = Stream
      .emit[IO, String](SummaryReport.toList.mkString(",")) // headerを作成
      .append(stream
        .through(summarize)
        .map(_.toString)
      )
      .intersperse(System.lineSeparator)
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path(writeCsvPath)))
      .compile
      .drain
      .>> { IO.println(s"Done Write CSV: $writeCsvPath") }