package com.github.imomushi8.systra.core.entity

/*--------------------------------------------------------------------------------------------*/
import java.time.LocalDateTime
/*--------------------------------------------------------------------------------------------*/

/** ポジション 1つに対する記録表 */
case class PositionTransaction(openTime: LocalDateTime,
                               closeTime: LocalDateTime,
                               side: Int,
                               size: Double,
                               openPrice: Double,
                               closePrice: Double,
                               cost: Double):
                            
  /** レポート要素を文字列にしたリスト */
  val toList: List[String] = {
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

  override def toString: String = toList.mkString(",")

object PositionTransaction:

  def makeReport(position:Position, closePrice:Double, closeTime:LocalDateTime) = new PositionTransaction(
    position.openTime,
    closeTime,
    position.side,
    position.size,
    position.price,
    closePrice, 0)

  val toList: List[String] = List(
    "OpenTime",
    "CloseTime",
    "Side",
    "Size",
    "OpenPrice",
    "ClosePrice",
    "Cost")

  val header: String = toList.mkString(",")