package com.github.imomushi8.systra

import com.github.imomushi8.systra.core.entity._

case class OHLCV(
  dateStr: String,
  timeStr: String,
  open   : Price,
  high   : Price,
  low    : Price,
  close  : Price,
  volume : Volume,
)