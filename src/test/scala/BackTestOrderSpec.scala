import com.github.imomushi8.systra._
import com.github.imomushi8.systra.backtest._
import com.github.imomushi8.systra.backtest.BTMarket._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.report.PositionReport

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{Configuration, TableDrivenPropertyChecks}
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckDrivenPropertyChecks}

import java.time.LocalDateTime
import fs2._
import cats.implicits._
import cats.kernel.Monoid
import org.atnos.eff.syntax.all._

class BackTestOrderSpec
  extends AnyFlatSpec
  with Configuration
  with Matchers
  with ScalaCheckDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Checkers:

  lazy val genTradeInfo: Gen[(List[Chart], List[Order], List[Order], List[Order], List[Position])] = 
  for
    charts <- Gen.nonEmptyListOf(genChart)
    orders <- Gen.nonEmptyListOf(genOrder)
    contractedOrders <- Gen.nonEmptyListOf(genContracted(charts.head, orders))
    positions <- Gen.nonEmptyListOf(genOrderNonCoveredPosition(orders ++ contractedOrders))
    settledOrders <- Gen.nonEmptyListOf(genSettle(charts.head, positions))
  yield
    (charts, contractedOrders >>= getNonContractedOrders(orders), contractedOrders, settledOrders, positions)

  val memory = Monoid[MockBrain.Memory].empty
  val brain = MockBrain[BTMarket](2)

  "checkAllContract(chart, orders, positions)" should "pass all tests for any arities" in forAll(genTradeInfo) {
    case (charts, nonContractedOrders, contractedOrders, settledOrders, positions) => 
      val orders = nonContractedOrders ++ contractedOrders ++ settledOrders
      if charts.nonEmpty && orders.nonEmpty && positions.nonEmpty then
        val seqId = (orders.map(_.id.toIntOption.getOrElse(0)) ++ positions.map(_.id.toIntOption.getOrElse(0))).max
        val market = BTMarket(100000, orders, positions, seqId, charts.head, 0)
        val initEff = BackTest.put[BackTest.Stack[MockBrain.Memory], (BTMarket, MockBrain.Memory)]((market, memory))
        val trade = BackTest.trade(brain)
        val stream = Stream(charts.tail*).fold(initEff){ case (effect, chart) => effect *> trade(chart) }
        
        stream.compile.drain
        /*.last.foreach{
            effect => BackTest.run(effect, (market, memory))
        }*/
  }