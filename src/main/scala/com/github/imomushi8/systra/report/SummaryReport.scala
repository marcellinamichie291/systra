package com.github.imomushi8.systra.report

/*--------------------------------------------------------------------------------------------*/
import scala.annotation.tailrec
/*--------------------------------------------------------------------------------------------*/

/** トレード終了時に行うレポート */
case class SummaryReport(traderName: String,
                         sampleSize: Int,
                         longReport: SummarySubReport,
                         shortReport: SummarySubReport,
                         allReport: SummarySubReport,
                         maximalDrawDown: Double,
                         consecutiveWinCount: Int,
                         consecutiveLoseCount: Int) extends Report {
  override val toList: List[String] =
    List(traderName, sampleSize.toString) ++
    longReport.toList ++
    shortReport.toList ++
    allReport.toList ++
    List(f"$maximalDrawDown%.3f", consecutiveWinCount.toString, consecutiveLoseCount.toString)
}


/*--------------------------------------------------------------------------------------------*/


object SummaryReport extends Report:

  /** PositionReportのListからレポートをまとめる */
  def makeReport(sampleSize:Int, traderName:String, records:List[PositionReport]): SummaryReport = {
    /* ロング、ショート、すべてでのSubReport */
    val longReport  = SummarySubReport.makeReport(records.filter(_.side>0))
    val shortReport = SummarySubReport.makeReport(records.filter(_.side<0))

    /* 利益・損失の時系列 */
    /* TODO: 同じタイミングで決済されたポジションの計算をまとめる必要がある */
    val plSeries:List[Double] = records.sortBy(_.closeTime).map(record =>
        record.side*(record.closePrice - record.openPrice)*record.size - record.cost)

    /* 連勝・連敗回数のリスト */
    val consecutiveList = makeConsecutiveList(plSeries)
    val consecutiveWins  = consecutiveList.filter(_>0)
    val consecutiveLoses = consecutiveList.filter(_<0)

    new SummaryReport(
      traderName,
      sampleSize,
      longReport,
      shortReport,
      longReport + shortReport,
      makeMaximalDrawDown(plSeries),
      if(consecutiveWins.isEmpty) 0 else consecutiveWins.max,
      -(if(consecutiveLoses.isEmpty) 0 else consecutiveLoses.min))
  }

  /** 最大ドローダウンを返す関数 */
  private def makeMaximalDrawDown(plSeries:List[Double]):Double = {
    val capitalSeries = plSeries.scanLeft(0.0)(_+_).tail                                  // 累積和をとって資産曲線を作る
    val maximumSeries = for(i <- capitalSeries.indices) yield capitalSeries.take(i+1).max // 資産曲線の各時点までで最大のもの
    /* 最大曲線と資産曲線を引いたもの（ドローダウン）のうち、最大のものを返す */
    (maximumSeries zip capitalSeries).map { case (maximum, capital) => maximum-capital }.maxOption.getOrElse(0)
  }

  /** 連勝を正の数、連敗を負の数で数えてリストで返す関数 */
  private def makeConsecutiveList(plSeries:List[Double]):List[Int] = {
    @tailrec
    def consecutiveCount(series:List[Double], consecutiveList:List[Int]):List[Int] = series match {
      case series if series.head > 0  => consecutiveCount(series.dropWhile(_>0),  series.takeWhile(_>0).size::consecutiveList)
      case series if series.head < 0  => consecutiveCount(series.dropWhile(_<0), -series.takeWhile(_<0).size::consecutiveList)
      case series if series.head == 0 => consecutiveCount(series.dropWhile(_==0), consecutiveList)
      case _                          => consecutiveList
    }

    consecutiveCount(plSeries, Nil)
  }

  /** レポート要素を文字列にしたリスト */
  override val toList: List[String] =
    List("TraderName", "SampleSize") ++
    SummarySubReport.toList.map("Long".concat) ++
    SummarySubReport.toList.map("Short".concat) ++
    SummarySubReport.toList ++
    List("MaximalDrawDown", "ConsecutiveWinCount", "ConsecutiveLoseCount")