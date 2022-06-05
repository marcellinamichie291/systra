package com.github.imomushi8.systra.core.market

trait Flag
case object OK extends Flag
case class  NG(msg: String) extends Flag
