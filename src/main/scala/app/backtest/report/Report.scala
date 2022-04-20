package com.github.imomushi8.systra.report

trait Report {
  def toList:List[String]

  override def toString: String = toList.mkString(",")
}
