package com.github.imomushi8.systra.core.util

case class SizeNegativeException(msg: String) extends Throwable(msg)
case class CapitalShortfallException(msg: String) extends Throwable(msg)
case class OrderCancelFailureException(msg: String) extends Throwable(msg)
case class NoSettlePositionException(msg: String) extends Throwable(msg)