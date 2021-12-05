package com.github.imomushi8.systra.util

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

type ID = String
type Side = Int
type Price = Double
type Size = Double
type Volume = Double
type TimeStamp = LocalDateTime

case class OHLCV(
  dateStr: String,
  timeStr: String,
  open   : Price,
  high   : Price,
  low    : Price,
  close  : Price,
  volume : Volume,
)

val datetimeFormatter    = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
val csvDatetimeFormatter = DateTimeFormatter.ofPattern("yyyy.MM.dd HH:mm")

inline val START  = "START  {}"
inline val END    = "END    {}"
inline val OPEN   = "OPEN   {}"
inline val CLOSE  = "CLOSE  {}"
inline val GET    = "GET    {}"
inline val CANCEL = "CANCEL {}"
inline val ORDER  = "ORDER  {}"

inline val BUY = 1
inline val SELL = -1
