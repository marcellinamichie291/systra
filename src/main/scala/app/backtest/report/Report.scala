package app.backtest.report

trait Report {
  def toList:List[String]

  override def toString: String = toList.mkString(",")
}
