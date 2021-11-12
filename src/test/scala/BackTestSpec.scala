import com.github.imomushi8.systra._
import com.github.imomushi8.systra.backtest._
import com.github.imomushi8.systra.backtest.{BackTest, BTMarket}
import com.github.imomushi8.systra.util._

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{Configuration, TableDrivenPropertyChecks}
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckDrivenPropertyChecks}

import java.time.LocalDateTime
import fs2._
import cats.kernel.Monoid
import com.github.imomushi8.systra.report.PositionReport

implicit lazy val arbClosed: Arbitrary[List[(Position, Price, TimeStamp)]] = Arbitrary( Gen.listOf {
  for
    position <- genPosition
    closePrice <- Gen.posNum[Price]
    closeTime <- genTimeStamp
  yield (position, closePrice, closeTime)
})

class BackTestSpec
  extends AnyFlatSpec
  with Configuration
  with Matchers
  with ScalaCheckDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Checkers:
  
  val initMarket: BTMarket = BTMarket(100, Nil, Nil, 1, null, 0)

  "makeReport" must "make PositionReport" in forAll { (positions: List[(Position, Price, TimeStamp)]) =>
    val reports = makeReport(positions)
    
    reports.size should equal (positions.size)
    reports.map(_.asInstanceOf[PositionReport].openPrice).toList should equal (positions.map(_._1.price))
    reports.map(_.asInstanceOf[PositionReport].closePrice).toList should equal (positions.map(_._2))
    reports.map(_.asInstanceOf[PositionReport].openTime).toList should equal (positions.map(_._1.openTime))
    reports.map(_.asInstanceOf[PositionReport].closeTime).toList should equal (positions.map(_._3))
    reports.map(_.asInstanceOf[PositionReport].side).toList should equal (positions.map(_._1.side))
    reports.map(_.asInstanceOf[PositionReport].size).toList should equal (positions.map(_._1.size))
  }

  "contract process" should "pass all test" in forAll {(chart: Chart, order: Order) =>
    val isSTLM = isSTOP_LIMIT(order)
    isSTLM should equal (order.price > 0 && order.triggerPrice > 0)

    val hasEvent = hasContractEvent(chart, order)
    if order.isLIMIT && order.isSTOP then // STOP_LIMIT
      if order.isBUY then
        if chart.high > order.triggerPrice then hasEvent should equal (true)
        else hasEvent should equal (false)
      else
        if chart.low <= order.triggerPrice then hasEvent should equal (true)
        else hasEvent should equal (false)
    
    if order.isSTOP then // STOP
      if order.isBUY then
        if chart.high > order.triggerPrice then hasEvent should equal (true)
        else hasEvent should equal (false)
      else
        if chart.low <= order.triggerPrice then hasEvent should equal (true)
        else hasEvent should equal (false)

    if order.isLIMIT && !order.isSTOP then // LIMIT
      if order.isBUY then
        if chart.low <= order.price then hasEvent should equal (true)
        else hasEvent should equal (false)
      else
        if chart.high > order.price then hasEvent should equal (true)
        else hasEvent should equal (false)

    val contractFlag = isContracted(chart)(order)
    
    contractFlag should equal (order.parentId == "" && (hasEvent || order.isMarket))

    val position = createPosition(chart)(order)
    position.id       should equal (order.id)
    position.openTime should equal (chart.datetime)
    if !order.isLIMIT &&  order.isSTOP then position.price should equal (order.triggerPrice)
    if  order.isLIMIT && !order.isSTOP then position.price should equal (order.price)
    position.side should equal (order.side)
    position.size should equal (order.size)
  }