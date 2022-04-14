package com.github.imomushi8.systra.backtest

import com.github.imomushi8.systra.core.data.ChartStream

import fs2._
import cats.effect.IO
import com.github.imomushi8.systra.core.entity.Chart

import com.github.gekomad.ittocsv.core.FromCsv.Decoder
import com.github.gekomad.ittocsv.parser.io.FromFile._
import com.github.gekomad.ittocsv.parser.IttoCSVFormat
import deriving.Mirror.ProductOf
import java.time.LocalDateTime

final case class BackTestStream[Csv](filePath:   String,
                                     csvToChart: Csv => Chart)
                                    (using p:    ProductOf[Csv],
                                           d:    Decoder[List[String], p.MirroredElemTypes],
                                           i:    IttoCSVFormat) extends ChartStream:
  def begin(): Stream[IO, Chart] =
    val chartStream = csvFromFileStream[Csv](filePath, skipHeader = false)
    
    val head = chartStream.head.map {
        case Left(line) => throw Exception()
        case Right(csv) => csvToChart(csv)
    }
    
    head.flatMap { headChart =>
      chartStream
      .scan(headChart) { (prev, csvEither) => csvEither match 
        case Left(_)    => prev
        case Right(csv) => csvToChart(csv)
      }
    }