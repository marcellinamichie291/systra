package com.github.imomushi8.systra.report

/*--------------------------------------------------------------------------------------------*/
import com.github.imomushi8.systra.entity.Position
import java.time.LocalDateTime
/*--------------------------------------------------------------------------------------------*/

/** ポジション 1つに対する記録表 */
case class PositionReport(openTime: LocalDateTime,
                          closeTime: LocalDateTime,
                          side: Int,
                          size: Double,
                          openPrice: Double,
                          closePrice: Double,
                          cost: Double) extends Report:
                            
  /** レポート要素を文字列にしたリスト */
  override val toList: List[String] = {
    val sideStr = if(side>0) "BUY" else "SELL"
    List(
      openTime.toString,
      closeTime.toString,
      sideStr,
      size.toString,
      openPrice.toString,
      closePrice.toString,
      cost.toString)
  }

object PositionReport extends Report:

  def makeReport(position:Position, closePrice:Double, closeTime:LocalDateTime) = new PositionReport(
    position.openTime,
    closeTime,
    position.side,
    position.size,
    position.price,
    closePrice, 0)

  override val toList: List[String] = List(
    "OpenTime",
    "CloseTime",
    "Side",
    "Size",
    "OpenPrice",
    "ClosePrice",
    "Cost")