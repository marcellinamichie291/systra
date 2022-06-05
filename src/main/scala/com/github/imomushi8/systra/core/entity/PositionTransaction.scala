package com.github.imomushi8.systra.core.entity

/*--------------------------------------------------------------------------------------------*/
import com.github.imomushi8.systra.core.entity.UnixTimeStamp._
import java.time.LocalDateTime
/*--------------------------------------------------------------------------------------------*/

/**
 * ポジション 1つに対する記録表
 * @param openTime
 *   TimeStamp
 * @param closeTime
 *   TimeStamp
 * @param side
 *   Side
 * @param size
 *   Size
 * @param openPrice
 *   Price
 * @param closePrice
 *   Price
 * @param cost
 *   Price
 */
case class PositionTransaction(openTime:   TimeStamp,
                               closeTime:  TimeStamp,
                               side:       Side,
                               size:       Size,
                               openPrice:  Price,
                               closePrice: Price,
                               cost:       Price):

  /**
   * レポート要素を文字列にしたリスト
   */
  val toList: List[String] = {
    val sideStr = if (side > 0) "BUY" else "SELL"
    List(
      openTime.show(),
      closeTime.show(),
      sideStr,
      size.toString,
      openPrice.toString,
      closePrice.toString,
      cost.toString)
  }

  override def toString: String = toList.mkString(",")

  
object PositionTransaction:

  def makeReport(position: Position, closePrice: Double, closeTime: TimeStamp) =
    new PositionTransaction(position.openTime, closeTime, position.side, position.size, position.price, closePrice, 0)

  val toList: List[String] = List("OpenTime", "CloseTime", "Side", "Size", "OpenPrice", "ClosePrice", "Cost")

  val header: String = toList.mkString(",")
