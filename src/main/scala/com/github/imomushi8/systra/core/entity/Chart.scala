package com.github.imomushi8.systra.core.entity

import com.github.imomushi8.systra.core.entity.UnixTimeStamp._
import cats.kernel.Monoid

/**
 * OHLCV形式のデータ
 * @param open
 *   Price
 * @param high:
 *   Price
 * @param low:
 *   Price
 * @param close:
 *   Price
 * @param volume:
 *   Volume
 * @param timestamp:
 *   TimeStamp
 */
case class Chart(open:      Price, 
                 high:      Price, 
                 low:       Price, 
                 close:     Price, 
                 volume:    Volume, 
                 timestamp: TimeStamp):

  override val toString: String =
    f"Chart(${timestamp.show()}, O:$open%.3f yen, H:$high%.3f yen, L:$low%.3f yen, C:$close%.3f yen, V:$volume%.3f)"
    // f"Chart(${datetime.format(datetimeFormatter)}, O:$open%.3f yen, H:$high%.3f yen, L:$low%.3f yen, C:$close%.3f yen, V:$volume%.3f)"

  def setTime(timestamp2: Long) = copy(timestamp = timestamp2.toTimeStamp)

given chartMonoid: Monoid[Chart] with {
  def empty: Chart = Chart(0, 0, 0, 0, 0, UnixTimeStamp.now)
  
  def combine(a: Chart, b: Chart) = Chart(a.open, a.high max b.high, a.low min b.low, b.close, a.volume + b.volume, a.timestamp)
}
