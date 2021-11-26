import com.github.imomushi8.systra._
import com.github.imomushi8.systra.backtest._
import com.github.imomushi8.systra.backtest.{BackTest, BTMarket}
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report.PositionReport

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{Configuration, TableDrivenPropertyChecks}
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckDrivenPropertyChecks}

import java.time.LocalDateTime
import fs2._
import cats.kernel.Monoid

/*************************************************************************************************/

class BackTestContractSpec
  extends AnyFlatSpec
  with Configuration
  with Matchers
  with ScalaCheckDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Checkers:

  implicit lazy val arbChart: Arbitrary[Chart] = Arbitrary(genChart)
  implicit lazy val arbOrder: Arbitrary[Order] = Arbitrary(genOrder)
  implicit lazy val arbPosition: Arbitrary[Position] = Arbitrary(genPosition)
  implicit lazy val arbOrderMethod: Arbitrary[OrderMethod] = Arbitrary(genOrderMethod)

  "isSTOP_LIMIT(order)" should "pass all tests" in forAll { (order: Order) =>
    isSTOP_LIMIT(order) should equal (order.price > 0 && order.triggerPrice > 0)
  }

  "isContracted(chart)(order)" should "pass all tests" in forAll {(chart: Chart, order: Order) =>
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
  }

/*************************************************************************************************/

  "createPosition(chart)(order)" should "pass all tests" in forAll {(chart: Chart, order: Order) =>
    val position = createPosition(chart)(order)
    position.id       should equal (order.id)
    position.openTime should equal (chart.datetime)
    if !order.isLIMIT &&  order.isSTOP then position.price should equal (order.triggerPrice)
    if  order.isLIMIT && !order.isSTOP then position.price should equal (order.price)
    position.side should equal (order.side)
    position.size should equal (order.size)
  }

  // TODO: 空リストだとIllegalArgumentExceptionがでてしまうのはなぜ？
  "settle(chart, positions)(contractOrder)" should "pass all tests" in forAll(genChart, Gen.nonEmptyListOf(genPosition)) {
    (chart: Chart, positions: List[Position]) => forAll(genSettle(chart, positions)) {
      (contractedOrder: Order) =>
        
        val settleList = settle(chart, positions)(contractedOrder)
        
        for(tuple <- settleList) {
          val position = tuple._1
          val closePrice = tuple._2
          val closeTime= tuple._3
          
          position.id should equal (contractedOrder.settlePositionId)
          positions should contain (position)
          if contractedOrder.isLIMIT then closePrice should equal (contractedOrder.price)
          if contractedOrder.isSTOP then closePrice should equal (contractedOrder.triggerPrice)
          closeTime should equal (chart.datetime)
        }
    }
  }

/*************************************************************************************************/

  "makeReport" should "make PositionReport" in {
    val genClosed: Gen[List[(Position, Price, TimeStamp)]] = Gen.listOf {
      for
        position <- genPosition
        closePrice <- Gen.posNum[Price]
        closeTime <- genTimeStamp
      yield (position, closePrice, closeTime)
    }

    forAll(genClosed) { (positions: List[(Position, Price, TimeStamp)]) =>
      val reports = makeReport(positions)
      reports.size should equal (positions.size)
      reports.map(_.asInstanceOf[PositionReport].openPrice).toList should equal (positions.map(_._1.price))
      reports.map(_.asInstanceOf[PositionReport].closePrice).toList should equal (positions.map(_._2))
      reports.map(_.asInstanceOf[PositionReport].openTime).toList should equal (positions.map(_._1.openTime))
      reports.map(_.asInstanceOf[PositionReport].closeTime).toList should equal (positions.map(_._3))
      reports.map(_.asInstanceOf[PositionReport].side).toList should equal (positions.map(_._1.side))
      reports.map(_.asInstanceOf[PositionReport].size).toList should equal (positions.map(_._1.size))
    }
  }