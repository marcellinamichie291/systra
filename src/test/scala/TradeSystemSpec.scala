import cats.kernel.Monoid

import MockBrain._
import com.github.imomushi8.systra._
import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.backtest.{BackTest, BTMarket}
import com.github.imomushi8.systra.util.{Chart, Order}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{Configuration, TableDrivenPropertyChecks}
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckDrivenPropertyChecks}

import java.time.LocalDateTime
import com.typesafe.scalalogging.LazyLogging

implicit lazy val arbChart: Arbitrary[Chart] = Arbitrary {
  for {
    high     <- Gen.posNum[Price]
    low      <- Gen.choose[Price](0, high)
    open     <- Gen.choose[Price](low, high)
    close    <- Gen.choose[Price](low, high)
    volume   <- Gen.posNum[Volume]
    datetime <- Gen.choose[TimeStamp](LocalDateTime.MIN, LocalDateTime.MAX)
  } yield Chart(open, high, low, close, volume, datetime)
}

class TradeSystemSpec 
  extends AnyFlatSpec
  with Configuration
  with Matchers
  with ScalaCheckDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Checkers:

  val initMarket: BTMarket = BTMarket(100, Nil, Nil, 1, null, 0)
  val brain: Brain[BTMarket, Memory] = MockBrain(1)

  "In MockBrain, BackTest.trade" should "success once for all chart." in forAll { (chart: Chart) =>
    val (next, log) = BackTest.trade(brain)(chart).run((initMarket, Monoid[Memory].empty)).value

    val market = next._1
    val orders = market.orders
    val nariyukiBUY   = Order("1", BUY,   chart.close, 0, 200.0, chart.datetime.plusMonths(1), Monoid[ID].empty, isMarket = true)
    val nariyukiSELL  = Order("1", SELL,  chart.close, 0, 200.0, chart.datetime.plusMonths(1), Monoid[ID].empty, isMarket = true )

    market.capital should equal (100.0)
    orders should contain atLeastOneOf (nariyukiBUY, nariyukiSELL)
    orders.size should equal (3)
    market.positions shouldBe empty
    market.sequenceId should equal (4)
    market.chart should equal (chart)

    log shouldBe Vector()
  }