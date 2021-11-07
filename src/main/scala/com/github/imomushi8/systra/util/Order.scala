package com.github.imomushi8.systra.util

/*--------------------------------------------------------------------------------------------*/
import cats.kernel.Monoid
import com.github.imomushi8.systra.{ID, Side, Price, Size, TimeStamp}
/*--------------------------------------------------------------------------------------------*/


/**
 * 1つの注文情報を保有するクラス
 * @param id 注文ID
 * @param side 方向（BUY:1, SELL:-1）
 * @param price 指値価格（LIMIT,STOP_LIMITの場合必須、それ以外は0）
 * @param triggerPrice 逆指値価格（STOP,STOP_LIMITの場合必須、それ以外は0）
 * @param size 数量
 * @param expire 有効期限
 * @param settlePositionId 決済する場合のポジションID（未指定の場合 Monoid[ID].empty を利用）
 * @param parentId 親注文のID（デフォルトでMonoid[ID].empty）
 * @param brotherId OCO注文のID（デフォルトでMonoid[ID].empty）
 */
case class Order(id               :ID,
                 side             :Side,
                 price            :Price,
                 triggerPrice     :Price,
                 size             :Size,
                 expire           :TimeStamp,
                 settlePositionId :ID,
                 parentId         :ID = Monoid[ID].empty,
                 brotherId        :ID = Monoid[ID].empty,
                 isMarket         :Boolean = false):
  /*------------------------------------------------------------------------------------------*/
  /* フィールド */

  val isBUY: Boolean = side > 0
  val isLIMIT: Boolean = price > 0
  val isSTOP: Boolean = triggerPrice > 0
  val hasBrother: Boolean = brotherId.nonEmpty
  val hasParent: Boolean = parentId.nonEmpty
  val isSettle: Boolean = settlePositionId.nonEmpty
  /*------------------------------------------------------------------------------------------*/
  /* 実装メソッド */

  override lazy val toString: String = {
    val ifdoco = if(hasParent && hasBrother) "IFDOCO " else if(hasParent) "IFD " else if(hasBrother) "OCO " else ""
    val methodStr = if(isMarket) "MARKET" else if(isLIMIT) if(isSTOP) "STOP_LIMIT" else "LIMIT" else "STOP"
    val priceStr = if(isLIMIT) s"ordered: ${price}yen, " else ""
    val triggerPriceStr = if(isSTOP) s"trigger: ${triggerPrice}yen, " else ""
    val sideStr = if(isBUY) "BUY" else "SELL"
    val settleStr = if(isSettle) s", settle Position ID: $settlePositionId" else ""
    s"${ifdoco}Order($id, $sideStr, $methodStr, $priceStr$triggerPriceStr$size amount$settleStr)"
  }

  /** STOP_LIMIT -> LIMIT に変換するために使用 */
  def invalidateTriggerPrice:Order =
    Order(id,side, price, 0, size, expire, settlePositionId, parentId, brotherId)

  /** IFD,IFDOCO注文を解除するために使用 */
  def invalidateParentId:Order =
    if(isSettle) // 約定先が存在すればそのまま流用
      Order(id, side, price, triggerPrice, size, expire, settlePositionId, "", brotherId)
    else // 約定先が設定されていなければparentIdを約定先に設定する
      Order(id, side, price, triggerPrice, size, expire, parentId, "", brotherId)