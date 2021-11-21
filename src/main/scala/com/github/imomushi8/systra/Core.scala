package com.github.imomushi8.systra
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

type ID = String
type Side = Int
type Price = Double
type Size = Double
type Volume = Double
type TimeStamp = LocalDateTime

val datetimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")

inline val START_MARKET  = "Start Market"
inline val STOP_MARKET   = "Stop  Market"
inline val OPEN          = "OPEN   {}"
inline val CLOSE         = "CLOSE  {}"
inline val GET           = "GET    {}"
inline val CANCEL        = "CANCEL {}"
inline val ORDER         = "ORDER  {}"

inline val BUY = 1
inline val SELL = -1
