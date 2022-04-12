package com.github.imomushi8.systra.core.util

import java.time.format.DateTimeFormatter

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
