package com.github.imomushi8.systra.chart

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
import java.time.temporal.TemporalAmount

object CsvStream:
  /**
   * CSVからChartを取得する
   */
  def apply[Csv](csvPath:    String)
                (csvToChart: Csv => Chart)
                (using p:    ProductOf[Csv],
                       d:    Decoder[List[String], p.MirroredElemTypes],
                       i:    IttoCSVFormat): Stream[IO, Chart] =
    val chartStream = csvFromFileStream[Csv](csvPath, skipHeader = false)
    val head        = chartStream.head.map {
      case Left(line) => throw Exception()
      case Right(csv) => csvToChart(csv)
    }

    head.flatMap { headChart =>
      chartStream.scan(headChart) { (prev, csvEither) =>
        csvEither match
          case Left(_)    => prev
          case Right(csv) => csvToChart(csv)
      }
    }

  def downSampling(period: TemporalAmount)
                  (stream: Stream[IO, Chart]): Stream[IO, Chart] =
    stream.head.flatMap { head =>
      stream
        .scan(head.asLeft[(Chart, Chart)]) {
          // Leftであれば更新していく
          case (Left(Chart(open, high, low, close, volume, datetime)), chart) =>
            if (chart.datetime compareTo datetime.plusMinutes(5)) < 0
            then // 規定時間内の場合
              Chart(
                open,
                high max chart.high,
                low min chart.low,
                chart.close,
                volume + chart.volume,
                datetime).asLeft[(Chart, Chart)]
            else
              (Chart(open, high, low, close, volume, datetime), chart)
                .asRight[Chart]

          // Rightであればリセットする
          case (Right(_, prev), Chart(_, high, low, close, volume, _)) =>
            Chart(
              prev.open,
              high max prev.high,
              low min prev.low,
              close,
              volume + prev.volume,
              prev.datetime).asLeft[(Chart, Chart)]
        }
        .collect { case (Right((chart, _))) => chart }
    }
