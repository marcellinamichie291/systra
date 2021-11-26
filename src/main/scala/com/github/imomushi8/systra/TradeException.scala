package com.github.imomushi8.systra

case class  SizeNegativeException(msg: String) extends Throwable(msg)
case class  CapitalShortfallException(msg: String) extends Throwable(msg)
case class  OrderCancelFailureException(msg: String) extends Throwable(msg)