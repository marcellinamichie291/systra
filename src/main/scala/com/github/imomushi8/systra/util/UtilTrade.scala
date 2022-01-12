package com.github.imomushi8.systra.util

import com.github.imomushi8
import scala.annotation.tailrec

object UtilTrade:
  /** 対数価格が正規分布に従うと仮定して、連続複利を最大化する賭け率　*/
  def optimalF(expectProfit:   Price,
               dualVolatility: Price,
               kellyWeight:    Double): Double = kellyWeight * expectProfit / dualVolatility

  /** 賭け比率と掛け金と注文価格から注文サイズを逆算 */
  def backwordSizeForCapital(ratio:      Double,
                             orderPrice: Price,
                             capital:    Price): Size = ratio * capital / orderPrice