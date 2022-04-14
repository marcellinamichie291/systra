import com.github.imomushi8.systra.backtest._
import com.github.imomushi8.systra.backtest.BackTestOrder._
import com.github.imomushi8.systra.backtest.BackTestPosition._
import com.github.imomushi8.systra.backtest.BackTestContract._

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.action._
import com.github.imomushi8.systra.core.market._
import com.github.imomushi8.systra.core.entity._

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{Configuration, TableDrivenPropertyChecks}
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckDrivenPropertyChecks}

import java.time.LocalDateTime
import fs2._
import cats.implicits._
import cats.kernel.Monoid

def getNonContractedOrders(orders:List[Order])(contractedOrder:Order) = orders.map { order =>
  if order.id == contractedOrder.brotherId then order.copy(brotherId = contractedOrder.id)
  else order
}

def genContractedPrice(side:Side, isSTOP: Boolean, chart:Chart): Gen[Price] = 
  if isSTOP then 
    if side>0 then Gen.choose(0, chart.high)
    else Gen.choose(chart.low, Int.MaxValue)
  else 
    if side>0 then Gen.choose(chart.low, Int.MaxValue)
    else Gen.choose(0, chart.high)


def genContracted(chart: Chart, orders:List[Order]): Gen[Order] =
  val maxId = orders.filter(_.id.nonEmpty).map(_.id.toInt).maxOption.getOrElse(1)
  val genNonOrdersId = Gen.choose(maxId+1, Int.MaxValue).map(_.toString)
  val genOrdersParentId = orders.collect { case order if order.id.nonEmpty => (1, Gen.const(order.parentId)) }
  val genOrdersId = orders.map(o=>Gen.freqTuple(1, o.id))

  for
    id <- Gen.frequency(((3, genNonOrdersId) :: genOrdersParentId)*)
    side <- genSide
    price <- genContractedPrice(side, false, chart)
    triggerPrice <- genContractedPrice(side, true, chart)
    size <- Gen.posNum[Size]
    expire <- genTimeStamp
    //settlePositionId <- genHasEmptyId
    brotherId <- Gen.frequency(((3, genHasEmptyId) :: genOrdersId)*)
  yield
    Order(id, side, price, triggerPrice, size, expire, "", "", brotherId)

def genSettle(chart: Chart, positions: List[Position]): Gen[Order] = 
  val maxId = positions.filter(_.id.nonEmpty).map(_.id.toInt).maxOption.getOrElse(1)
  val genNonPositionsId = Gen.choose(maxId+1, Int.MaxValue).map(_.toString)
  val genPositionsId = positions.filter(_.id.nonEmpty).map(p=>Gen.freqTuple(1, p.id))
  for
    id <- genNonPositionsId
    side <- genSide
    price <- genContractedPrice(side, false, chart)
    triggerPrice <- genContractedPrice(side, true, chart)
    size <- Gen.posNum[Size]
    expire <- Gen.choose[TimeStamp](LocalDateTime.MIN, chart.datetime)
    settlePositionId <- Gen.frequency(genPositionsId*)
    brotherId <- genHasEmptyId
  yield
    val order = Order(id, side, price, triggerPrice, size, expire, settlePositionId, "", brotherId)
    if isSTOP_LIMIT(order) then order.invalidateTriggerPrice
    else order

def genOrderNonCoveredPosition(orders: List[Order]): Gen[Position] =
  val maxId = orders.filter(_.id.nonEmpty).map(_.id.toInt).maxOption.getOrElse(1)
  val genNonOrdersId = Gen.choose(maxId+1, Int.MaxValue).map(_.toString)
  for
    openTime <- genTimeStamp
    id <- genNonOrdersId
    side <- genSide
    price <- Gen.posNum[Price]
    size <- genSize
  yield Position(openTime, id, side, price, size)

lazy val genMarketContext: Gen[(Chart, List[Order], List[Order], List[Order], List[Position])] = 
  for
    chart <- genChart
    orders <- Gen.listOf(genOrder)
    contractedOrders <- Gen.listOf(genContracted(chart, orders))
    positions <- Gen.nonEmptyListOf(genOrderNonCoveredPosition(orders ++ contractedOrders))
    settledOrders <- Gen.listOf(genSettle(chart, positions))
  yield
    (chart, contractedOrders >>= getNonContractedOrders(orders), contractedOrders, settledOrders, positions)
