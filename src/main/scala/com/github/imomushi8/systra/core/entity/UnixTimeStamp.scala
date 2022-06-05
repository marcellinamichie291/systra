package com.github.imomushi8.systra.core.entity

import java.util.Date
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZonedDateTime}
import scala.concurrent.duration.{DurationLong, FiniteDuration}

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
    def toLong: Long = x

    def toDuration: FiniteDuration = x.millis

    /** 時刻表示を行う */
    def show(formatter: String = DEFAULT_FORMATTER): String = formatter format Date(x)
  }

  extension (x: Long) {

    /** 時刻表示を行う */
    def showDateTime(formatter: String = DEFAULT_FORMATTER): String = formatter format Date(x)

    def toTimeStamp: UnixTimeStamp = x
    def toSecond: UnixTimeStamp    = x * SECOND
    def toMinute: UnixTimeStamp    = x * MINUTE
    def toHour: UnixTimeStamp      = x * HOUR
    def toDay: UnixTimeStamp       = x * DAY

    /** 30日 */
    def toMonth: UnixTimeStamp = x * MONTH

    /** 365日 */
    def toYear: UnixTimeStamp = x * YEAR
  }

  implicit val numeric: Numeric[UnixTimeStamp] = Numeric[Long]
