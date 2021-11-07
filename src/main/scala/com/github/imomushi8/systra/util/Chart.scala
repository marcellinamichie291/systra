package com.github.imomushi8.systra.util

import com.github.imomushi8.systra.{Price, Volume, TimeStamp}

/** OHLCV形式のデータ */
case class Chart(open       :Price,
                 high       :Price,
                 low        :Price,
                 close      :Price,
                 volume     :Volume,
                 datetime   :TimeStamp)