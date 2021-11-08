import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._

import org.scalacheck.{Arbitrary, Gen}
import java.time.LocalDateTime

val genTimeStamp = Gen.choose[TimeStamp](LocalDateTime.MIN, LocalDateTime.MAX)
val genSide = Gen.oneOf[Side](BUY, SELL)

implicit lazy val arbChart: Arbitrary[Chart] = Arbitrary {
  for {
    high     <- Gen.posNum[Price]
    low      <- Gen.choose[Price](0, high)
    open     <- Gen.choose[Price](low, high)
    close    <- Gen.choose[Price](low, high)
    volume   <- Gen.posNum[Volume]
    datetime <- genTimeStamp
  } yield Chart(open, high, low, close, volume, datetime)
}

implicit lazy val arbOrder: Arbitrary[Order] = Arbitrary {
  for {
    id <- Gen.numStr
    side <- genSide
    price <- Gen.posNum[Price]
    triggerPrice <- Gen.posNum[Price]
    size <- Gen.posNum[Size]
    expire <- genTimeStamp
    settlePositionId <- Gen.numStr
    parentId <- Gen.numStr
    brotherId <- Gen.numStr
    isMarket <- Arbitrary.arbitrary[Boolean]
  } yield Order(id, side, price, triggerPrice, size, expire, settlePositionId, parentId, brotherId, isMarket)
}

implicit lazy val arbPosition: Arbitrary[Position] = Arbitrary {
  for {
    openTime <- genTimeStamp
    id <- Gen.numStr
    side <- genSide
    price <- Gen.posNum[Price]
    size <- Gen.posNum[Size]
  } yield Position(openTime, id, side, price, size)
}

implicit lazy val arbOrderMethod: Arbitrary[OrderMethod] = Arbitrary {
  for {
    side <- genSide
    price <- Gen.frequency[Price]((10, Gen.const(0)),(1, Gen.posNum[Price]))
    triggerPrice <- Gen.frequency[Price]((10, Gen.const(0)),(1, Gen.posNum[Price]))
    positionId <- Gen.numStr
  } yield {
      if price == 0 then
        if triggerPrice == 0 then
          MARKET(side, positionId)
        else
          STOP(side, triggerPrice, positionId)
      else
        if triggerPrice == 0 then
          LIMIT(side, price, positionId)
        else
          STOP_LIMIT(side, price, triggerPrice, positionId)
  }
}