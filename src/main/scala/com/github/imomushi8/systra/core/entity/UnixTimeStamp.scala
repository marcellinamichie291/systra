package com.github.imomushi8.systra.core.entity

import java.util.Date
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZonedDateTime}


opaque type UnixTimeStamp = Long

object UnixTimeStamp:
  inline val SECOND = 1000L
  inline val MINUTE = 60 * SECOND
  inline val HOUR   = 60 * MINUTE
  inline val DAY    = 24 * HOUR
  inline val MONTH  = 30 * DAY
  inline val YEAR   = 365 * DAY

  inline val DEFAULT_FORMATTER = "%tY-%<tm-%<td %<tH:%<tM:%<tS %<tZ %<tz"

  def apply(value: Long): UnixTimeStamp = value

  def parseSimple(dateStr: String): UnixTimeStamp = Instant.parse(dateStr).getEpochSecond

  def parse(dateStr: String, formatter: DateTimeFormatter): UnixTimeStamp =
    ZonedDateTime.parse(dateStr, formatter).toInstant.getEpochSecond

  def now: UnixTimeStamp = System.currentTimeMillis

  extension (x: UnixTimeStamp) {
    /** 時刻表示を行う */
    def show(formatter: String = DEFAULT_FORMATTER): String = formatter format Date(x)
  }

  extension (x: Long) {
    /** 時刻表示を行う */
    def showDateTime(formatter: String = DEFAULT_FORMATTER): String = formatter format Date(x)
    def toTimeStamp: UnixTimeStamp = x
    def toSecond: UnixTimeStamp = x*SECOND
    def toHour: UnixTimeStamp = x*HOUR
    def toDay: UnixTimeStamp = x*DAY
    /** 30日 */
    def toMonth: UnixTimeStamp = x*MONTH
    /** 365日 */
    def toYear: UnixTimeStamp = x*YEAR
  }

  implicit val numeric: Numeric[UnixTimeStamp] = Numeric[Long]

  /*new Numeric {
    def plus(x: UnixTimeStamp, y: UnixTimeStamp): UnixTimeStamp = x + y
    def minus(x: UnixTimeStamp, y: UnixTimeStamp): UnixTimeStamp = x - y
    def times(x: UnixTimeStamp, y: UnixTimeStamp): UnixTimeStamp = x * y
    def negate(x: UnixTimeStamp): UnixTimeStamp = -x
    def toInt(x: UnixTimeStamp): Int  = x.toInt
    def toLong(x: UnixTimeStamp): Long = x
    def toFloat(x: UnixTimeStamp): Float = x.toFloat
    def toDouble(x: UnixTimeStamp): Double = x.toDouble
    def fromInt(x: Int): UnixTimeStamp = x.toLong
    def compare(x: UnixTimeStamp, y: UnixTimeStamp): Int = x compare y
  }*/