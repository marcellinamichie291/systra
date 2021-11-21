import com.github.imomushi8.systra._
import com.github.imomushi8.systra.brain._
import com.github.imomushi8.systra.Actions.Brain
import com.github.imomushi8.systra.backtest.{BackTest, BTMarket}
import com.github.imomushi8.systra.util.{Chart, Order, MARKET}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{Configuration, TableDrivenPropertyChecks}
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckDrivenPropertyChecks}

import java.time.LocalDateTime
import fs2._
import cats.kernel.Monoid
import cats.effect.{ExitCode, IO}
import cats.data.EitherT
import cats.implicits.catsSyntaxFlatMapOps
import cats.effect.unsafe.implicits.global

class Fs2Spec 
  extends AnyFlatSpec
  with Configuration
  with Matchers
  with ScalaCheckDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Checkers:

  val initMarket: BTMarket = BTMarket(100, Nil, Nil, 1, null, 0)
  
  "test" should "failed" in {
    val chart = Chart(1,1,1,1,1,LocalDateTime.now)
    val size = 10
    val expire = chart.datetime.plusMonths(1)
    val ctx = initMarket.context

    (for {
      refMarket <- ctx.getMarket
      id <- refMarket.placeOrder(MARKET(SELL), size, expire)
    } yield id)
    
    ctx.market should not equal (initMarket)


  }
/*
  val initMemory: Memory = Monoid[Memory].empty
  val brain: Brain[BTMarket, Memory] = MockBrain(1)
  "In MockBrain, BackTest.run" should "success for all unit Stream" in forAll { (chart: Chart) =>
    val stream = Stream.emit[Pure, Chart](chart)

    val reportStream = BackTest(brain, (initMarket, initMemory)).run(stream)
    reportStream.toVector shouldBe Vector(Vector())
  }
*/