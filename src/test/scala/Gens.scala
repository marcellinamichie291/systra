import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.virtual.VirtualOrder._
import com.github.imomushi8.systra.virtual.VirtualPosition._
import com.github.imomushi8.systra.virtual.VirtualContract._

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.action._
import com.github.imomushi8.systra.core.market._
import com.github.imomushi8.systra.core.entity._

import org.scalacheck.{Arbitrary, Gen}
import java.time.LocalDateTime

lazy val genHasEmptyId = Gen.frequency((1, ""), (1, Gen.posNum[Short].map(_.toString)))
lazy val genHasEmptyPrice = Gen.frequency((1, Gen.const(0.0)), (1, Gen.posNum[Price]))

lazy val genTimeStamp = Gen.choose[TimeStamp](LocalDateTime.MIN, LocalDateTime.MAX)
lazy val genSide = Gen.oneOf[Side](BUY, SELL)
lazy val genSize = Gen.posNum[Size]

lazy val genChart: Gen[Chart] = 
  for
    high     <- Gen.posNum[Price]
    low      <- Gen.choose[Price](0, high)
    open     <- Gen.choose[Price](low, high)
    close    <- Gen.choose[Price](low, high)
    volume   <- Gen.posNum[Volume]
    datetime <- genTimeStamp
  yield Chart(open, high, low, close, volume, datetime)

lazy val genOrder: Gen[Order] = 
  for
    id <- Gen.choose[Short](0, Short.MaxValue) map (_.toString)
    side <- genSide
    price <- genHasEmptyPrice
    triggerPrice <- genHasEmptyPrice
    size <- genSize
    expire <- genTimeStamp
    settlePositionId <- genHasEmptyId
    parentId <- genHasEmptyId
    brotherId <- genHasEmptyId
  yield
    Order(id, side, price, triggerPrice, size, expire, settlePositionId, parentId, brotherId)

lazy val genPosition: Gen[Position] = 
  for
    openTime <- genTimeStamp
    id <- Gen.posNum[Int] map (_.toString)
    side <- genSide
    price <- Gen.posNum[Price]
    size <- genSize
  yield Position(openTime, id, side, price, size)

lazy val genOrderMethod: Gen[OrderMethod] = 
  for
    side <- genSide
    price <- genHasEmptyPrice
    triggerPrice <- genHasEmptyPrice
    positionId <- genHasEmptyId
  yield
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