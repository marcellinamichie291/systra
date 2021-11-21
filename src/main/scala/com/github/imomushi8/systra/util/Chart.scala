package com.github.imomushi8.systra.util

import com.github.imomushi8.systra._

/** OHLCV形式のデータ */
case class Chart(open       :Price,
                 high       :Price,
                 low        :Price,
                 close      :Price,
                 volume     :Volume,
                 datetime   :TimeStamp):
  override val toString: String = 
    f"Chart(${datetime.format(datetimeFormatter)}, O:$open%.3f yen, H:$high%.3f yen, L:$low%.3f yen, C:$close%.3f yen, V:$volume%.3f)"