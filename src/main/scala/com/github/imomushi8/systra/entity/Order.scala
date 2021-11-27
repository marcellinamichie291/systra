package com.github.imomushi8.systra.entity

/*--------------------------------------------------------------------------------------------*/
import cats.kernel.Monoid
import com.github.imomushi8.systra.util.{ID, Side, Price, Size, TimeStamp}
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

  override lazy val toString: String =
    val ifdoco = if hasParent && hasBrother then "IFDOCO " else if hasParent then "IFD " else if hasBrother then "OCO " else ""
    val methodStr = if isMarket then "MARKET" else if isLIMIT then if isSTOP then "STOP_LIMIT" else "LIMIT" else "STOP"
    val priceStr = if isLIMIT then f"ordered: $price%.3f yen, " else ""
    val triggerPriceStr = if isSTOP then f"trigger: $triggerPrice%.3f yen, " else ""
    val sideStr = if isBUY then "BUY" else "SELL"
    val settleStr = if isSettle then s", settle Position ID: $settlePositionId" else ""
    val parentStr = if hasParent then s", parent Position ID: $parentId" else ""
    val brotherStr = if hasBrother then s", brother Position ID: $brotherId" else ""
    f"${ifdoco}Order(ID: $id, $sideStr, $methodStr, $priceStr$triggerPriceStr$size%.3f amount$settleStr$parentStr$brotherStr)"

extension (order: Order)
  /** STOP_LIMIT -> LIMIT に変換するために使用 */
  def invalidateTriggerPrice: Order = order.copy(triggerPrice = 0)
    //Order(id,side, price, 0, size, expire, settlePositionId, parentId, brotherId)

  /** IFD,IFDOCO注文を解除するために使用 */
  def invalidateParentId: Order =
    if order.isSettle then order.copy(parentId = Monoid[ID].empty) // 約定先が存在すればそのまま流用
    else order.copy(settlePositionId = order.parentId, parentId = Monoid[ID].empty) // 約定先が設定されていなければparentIdを約定先に設定する