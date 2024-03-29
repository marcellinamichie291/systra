import app.brain.MockBrain
import com.github.imomushi8.systra.virtual._
import com.github.imomushi8.systra.virtual.VirtualMarket._
import com.github.imomushi8.systra.virtual.VirtualOrder._
import com.github.imomushi8.systra.virtual.VirtualPosition._
import com.github.imomushi8.systra.virtual.VirtualContract._

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

class BackTestOrderSpec
  extends AnyFlatSpec
  with Configuration
  with Matchers
  with ScalaCheckDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Checkers:

  lazy val genTradeInfo: Gen[(List[Chart], List[Order], List[Order], List[Order], List[Position])] = 
  for
    charts <- Gen.listOfN(3, genChart)
    orders <- Gen.listOfN(2, genOrder)
    contractedOrders <- Gen.listOfN(2, genContracted(charts.head, orders))
    positions <- Gen.listOfN(2, genOrderNonCoveredPosition(orders ++ contractedOrders))
    settledOrders <- Gen.listOfN(2, genSettle(charts.head, positions))
  yield
    (charts, contractedOrders >>= getNonContractedOrders(orders), contractedOrders, settledOrders, positions)

  val memory = Monoid[MockBrain.Memory].empty
  val brain = MockBrain[VirtualMarket](2)
/*
  "checkAllContract(chart, orders, positions)" should "pass all tests for any arities" in forAll(genTradeInfo) {
    case (charts, nonContractedOrders, contractedOrders, settledOrders, positions) => 
      val orders = nonContractedOrders ++ contractedOrders ++ settledOrders
      if charts.nonEmpty && orders.nonEmpty && positions.nonEmpty then
        val seqId = (orders.map(_.id.toIntOption.getOrElse(0)) ++ positions.map(_.id.toIntOption.getOrElse(0))).max
        val market = BTMarket(100000, orders, positions, seqId, charts.head, 0)
        val initEff = BackTest.put[BackTest.Stack[MockBrain.Memory], (BTMarket, MockBrain.Memory)]((market, memory))
        val trade = BackTest.trade(brain)
        val stream = Stream(charts.tail*)//.fold(initEff){ case (effect, chart) => effect *> trade(chart) }

        val effects = stream.compile.fold(initEff){ case (effect, chart) => effect *> trade(chart) }
        val res = BackTest.run(effects, (market, memory))
        res.foreach {
          case ((_, state), reports) =>
            println(reports.mkString("\n"))
        }
  }
  */