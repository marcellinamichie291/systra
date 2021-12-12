package com.github.imomushi8.systra.report


/** 勝ち数、利益、費用などをまとめたもの */
case class SummarySubReport(winCount: Int,
                            loseCount: Int,
                            profit: Double,
                            loss: Double,
                            cost: Double) extends Report {
  val tradeCount: Int = winCount + loseCount
  val pl: Double = profit + loss - cost
  val winRate:Double  = if(tradeCount!=0) 100*winCount/tradeCount else -1
  val loseRate:Double = if(tradeCount!=0) 100*loseCount/tradeCount else -1
  val profitFactor: Double = if(loss+cost!=0) profit/(loss+cost) else profit

  override val toList: List[String] = List(
    tradeCount.toString,
    winCount.toString,
    loseCount.toString,
    f"$winRate%.3f",
    f"$loseRate%.3f",
    f"$pl%.3f",
    f"$profit%.3f",
    f"$loss%.3f",
    f"$cost%.3f",
    f"$profitFactor%.3f")

  /** 足し算できるように設定 */
  def +(that: SummarySubReport): SummarySubReport = new SummarySubReport(
    winCount + that.winCount,
    loseCount + that.loseCount,
    profit + that.profit,
    loss + that.loss,
    cost + that.cost)
}

extension (sub:SummarySubReport)
  def +=(pl: Double, cost: Double):SummarySubReport = if pl > 0 then
    sub.copy(winCount = sub.winCount+1, profit = sub.profit+pl, cost = sub.cost+cost)
  else
    sub.copy(loseCount = sub.loseCount+1, loss = sub.loss+pl, cost = sub.cost+cost)

/*--------------------------------------------------------------------------------------------*/

object SummarySubReport extends Report {

  /** PositionRecordのListからレポートをまとめる */
  def makeReport(records:List[PositionReport]): SummarySubReport = {
    val plList:List[Double] = records.map( record =>
      record.side*(record.closePrice - record.openPrice)*record.size )
    val profits = plList.filter(_>=0)
    val losses = plList.filterNot(_>0)

    new SummarySubReport(profits.size, losses.size, profits.sum, losses.sum, records.map(_.cost).sum)
  }

  /** レポート要素を文字列にしたリスト */
  override val toList: List[String] = List(
    "TradeCount",
    "WinCount",
    "LoseCount",
    "WinRate",
    "LoseRate",
    "P/L",
    "Profit",
    "Loss",
    "Cost",
    "ProfitFactor")
}