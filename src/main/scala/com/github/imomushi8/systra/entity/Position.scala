package com.github.imomushi8.systra.entity

/*--------------------------------------------------------------------------------------------*/
import com.github.imomushi8.systra.util.{BUY, SELL, ID, Side, Price, Size, TimeStamp}
/*--------------------------------------------------------------------------------------------*/


/**
 * 建玉情報を保有するクラス
 * @param openTime エントリー時間
 * @param id ポジションID
 * @param side 方向（BUY:1, SELL:-1）
 * @param price エントリー価格
 * @param size 数量
 */
case class  Position(openTime :TimeStamp,
                     id       :ID,
                     side     :Side,
                     price    :Price,
                     size     :Size) {
  override val toString: String = {
    val sideStr = if(side>0) "BUY" else "SELL"
    f"Position($sideStr, $price%.3f yen, $size%.3f amount, ID: $id)"
  }
}

extension (position: Position)
  /** ポジションと逆サイドを返す */
  def oppositeSide: Side = if position.side > 0 then SELL else BUY