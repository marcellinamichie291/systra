package com.github.imomushi8.systra.util

/*--------------------------------------------------------------------------------------------*/
import cats.kernel.Monoid
import com.github.imomushi8.systra.{ID, Price, Side}
/*--------------------------------------------------------------------------------------------*/


/** 注文方法のデータ型 */
sealed trait OrderMethod

/** 子注文専用のデータ型 */
sealed trait ChildOrderMethod extends OrderMethod

/*--------------------------------------------------------------------------------------------*/
/* 単一注文 */

/** 成行注文（単一注文、IFDやOCO注文の子注文としては使えない）
 * @param side BUY or SELL
 * @param positionId 約定させるポジションのID（約定注文でなければ空でOK）
 */
case class MARKET(side        :Side,
                  positionId  :ID = Monoid[ID].empty) extends OrderMethod:
  override lazy val toString: String = {
    val sideStr = if (side > 0) "BUY" else "SELL"
    val positionStr = if (positionId.nonEmpty) s", positionID: $positionId" else ""
    s"MARKET($sideStr$positionStr)"
  }

/** 指値注文（単一注文）
 * @param side BUY or SELL
 * @param price 指値価格
 * @param positionId 約定させるポジションのID（約定注文でなければ空でOK）
 */
case class LIMIT(side       :Side,
                 price      :Price,
                 positionId :ID = Monoid[ID].empty) extends ChildOrderMethod:
  override lazy val toString: String = {
    val sideStr = if (side > 0) "BUY" else "SELL"
    val positionStr = if (positionId.nonEmpty) s", positionID: $positionId" else ""
    s"LIMIT($sideStr, price: $price yen$positionStr)"
  }

/** 逆指値注文（単一注文）
 * @param side BUY or SELL
 * @param triggerPrice 逆指値価格
 * @param positionId 約定させるポジションのID（約定注文でなければ空でOK）
 */
case class STOP(side          :Side,
                triggerPrice  :Price,
                positionId    :ID = Monoid[ID].empty) extends ChildOrderMethod:
  override lazy val toString: String = {
    val sideStr = if (side > 0) "BUY" else "SELL"
    val positionStr = if (positionId.nonEmpty) s", positionID: $positionId" else ""
    s"STOP($sideStr, trigger: $triggerPrice yen$positionStr)"
  }

/** STOP LIMIT注文（単一注文）
 * @param side BUY or SELL
 * @param price 指値価格
 * @param triggerPrice 逆指値価格
 * @param positionId 約定させるポジションのID（約定注文でなければ空でOK）
 */
case class STOP_LIMIT(side          :Side,
                      price         :Price,
                      triggerPrice  :Price,
                      positionId    :ID = Monoid[ID].empty) extends ChildOrderMethod:
  override lazy val toString: String = {
    val sideStr = if(side > 0) "BUY" else "SELL"
    val positionStr = if(positionId.nonEmpty) s", positionID: $positionId" else ""
    s"STOP_LIMIT($sideStr, price: $price yen, trigger: $triggerPrice yen$positionStr)"
  }

/*--------------------------------------------------------------------------------------------*/
/* 特殊注文 */

/** OCO注文（特殊注文）
 * @param upperMethod 上向き注文(子注文のみ指定可能)
 * @param lowerMethod 下向き注文(子注文のみ指定可能)
 */
case class OCO(upperMethod: ChildOrderMethod,
               lowerMethod: ChildOrderMethod) extends OrderMethod:
  override lazy val toString: String = s"OCO($upperMethod, $lowerMethod)"

/** IFD注文（特殊注文）
 * @param parentMethod 先注文(子注文のみ指定可能)
 * @param childMethod 後注文(子注文のみ指定可能)*/
case class IFD(parentMethod: ChildOrderMethod,
               childMethod: ChildOrderMethod) extends OrderMethod:
  override lazy val toString: String = s"IFD($parentMethod, $childMethod)"

/** IFDOCO注文（特殊注文）
 * @param parentMethod 先注文(子注文のみ指定可能)
 * @param upperMethod 上向き注文(子注文のみ指定可能)
 * @param lowerMethod 下向き注文(子注文のみ指定可能)
 */
case class IFDOCO(parentMethod: ChildOrderMethod,
                  upperMethod: ChildOrderMethod,
                  lowerMethod: ChildOrderMethod) extends OrderMethod:
  override lazy val toString: String = s"IFDOCO($parentMethod, $upperMethod, $lowerMethod)"

/** トレイル注文（特殊注文） */
case class TRAIL() extends OrderMethod