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

class BackTestContractSpec2
  extends AnyFlatSpec
  with Configuration
  with Matchers
  with ScalaCheckDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Checkers:
/*
  "checkAllContract(chart, orders, positions)" should "pass all tests for any arities" in forAll(genChart, Gen.listOf(genOrder), Gen.listOf(genPosition)) {
    (chart: Chart, orders: List[Order], positions:List[Position]) =>
      val (nextOrder, nextPositions, closed) = checkAllContract(chart, orders, positions)
      for(order <- orders.filter(_.id.nonEmpty)) {
        if isContracted(chart)(order) then
          nextOrder should not contain (order)

          if isSTOP_LIMIT(order) then
            nextOrder should contain (order.invalidateTriggerPrice)
          else
            if order.isSettle then
              nextPositions.map(_.id) should not contain (order.settlePositionId)
              closed should contain allElementsOf (settle(chart, positions)(order))
            else 
              nextPositions should contain (createPosition(chart)(order))

        else
          nextOrder should contain (order)
      }
    }
*/

  "checkAllContract(chart, orders, positions)" should "pass all tests" in forAll(genMarketContext) {
    case (chart, nonContractedOrders, contractedOrders, settledOrders, positions) => 
      
      val orders = (nonContractedOrders ++ contractedOrders ++ settledOrders).map {order => 
        if order.id == "" then order.copy(id="-1") else order
      }.distinct

      val contracted = orders filter isContracted(chart)
      val nonContracted = orders diff contracted
      val (nextOrders, nextPositions, reports) = checkAllContract(chart, orders, positions)
      
      for(order <- orders) {
        if isContracted(chart)(order) then
          nextOrders should not contain (order)

          if isSTOP_LIMIT(order) then
            nextOrders should contain (order.invalidateTriggerPrice)
          else
            if order.isSettle then
              nextPositions.map(_.id) should not contain (order.settlePositionId)
              reports should contain allElementsOf (makeReport(settle(chart, positions)(order)))
            else 
              nextPositions should contain (createPosition(chart)(order))
              nextOrders should not contain (order)
              val specificRelated = getRelated(nonContracted)(order)

              nextOrders should contain allElementsOf (convertOrders(specificRelated)(order))
              if specificRelated.nonEmpty then nextOrders should not contain allElementsOf (specificRelated)

        else
          val parentContracted  = contracted filter (order.parentId == _.id)
          val brotherContracted = contracted filter (order.brotherId == _.id)
          if (parentContracted filterNot isSTOP_LIMIT).nonEmpty then 
            nextOrders should contain (order.invalidateParentId)
          if (parentContracted filter isSTOP_LIMIT).nonEmpty then 
            nextOrders should not contain (order)
          if brotherContracted.nonEmpty then 
            nextOrders should not contain (order)
          if parentContracted.isEmpty && brotherContracted.isEmpty then 
            nextOrders should contain (order)
      }
  }


  "getRelated(nonContractedOrders.toSet)(contractedOrder)" should "pass all tests" in {
    forAll(genChart, Gen.listOf(genOrder)) { (chart: Chart, orders: List[Order]) =>
      forAll(genContracted(chart, orders)) { (contractedOrder: Order) =>
        if contractedOrder.id.nonEmpty then
          val nonContractedOrders = getNonContractedOrders(orders)(contractedOrder)
          val relatedOrders = getRelated(nonContractedOrders)(contractedOrder)
          
          nonContractedOrders foreach { nonContractedOrder =>
            if nonContractedOrder.parentId == contractedOrder.id then
              relatedOrders should contain (nonContractedOrder)
            else if nonContractedOrder.brotherId == contractedOrder.id then
              relatedOrders should contain (nonContractedOrder)
            else 
              relatedOrders should not contain (nonContractedOrder)
          }
      }
    }
  }

  "convertOrders(relatedOrders)(contractedOrder)" should "pass all tests" in {
    forAll(genChart, Gen.listOf(genOrder)) { (chart: Chart, orders: List[Order]) =>
      forAll(genContracted(chart, orders)) { (contractedOrder: Order) =>
        if contractedOrder.id.nonEmpty then
          val nonContractedOrders = getNonContractedOrders(orders)(contractedOrder) 
          val relatedOrders = getRelated(nonContractedOrders)(contractedOrder)
          val convertedOrders = convertOrders(relatedOrders)(contractedOrder)
            
          if isSTOP_LIMIT(contractedOrder) then
            convertedOrders should contain (contractedOrder.invalidateTriggerPrice)
          else
            convertedOrders should not contain (contractedOrder)
            if nonContractedOrders.exists(_.parentId == contractedOrder.id) then
              val filteredOrders = nonContractedOrders.filter(_.parentId == contractedOrder.id).map(_.invalidateParentId)
              convertedOrders should contain allElementsOf (filteredOrders)
            
            if nonContractedOrders.exists(_.brotherId == contractedOrder.id) then
              val filteredOrders = nonContractedOrders.filter(_.brotherId == contractedOrder.id)
              convertedOrders should not contain allElementsOf (filteredOrders)
      }
    }
  }

  "getNextOrders(contractedOrders)(nonContractedOrders)" should "pass all tests" in forAll(genMarketContext) {
    case (chart, nonContractedOrders, contractedOrders, _, _) => 
      val orders = (nonContractedOrders ++ contractedOrders).map {order => 
        if order.id == "" then order.copy(id="-1") else order
      }.distinct

      val contracted = orders filter isContracted(chart)
      val nonContracted = orders diff contracted
      val nextOrders = getNextOrders(contracted)(nonContracted)

      for(order <- orders) {
        if isContracted(chart)(order) then
          if isSTOP_LIMIT(order) then
            nextOrders should contain (order.invalidateTriggerPrice)
          else
            nextOrders should not contain (order)
            val specificRelated = getRelated(nonContracted)(order)

            nextOrders should contain allElementsOf (convertOrders(specificRelated)(order))
            if specificRelated.nonEmpty then nextOrders should not contain allElementsOf (specificRelated)
        
        else
          val parentContracted  = contracted filter (order.parentId == _.id)
          val brotherContracted = contracted filter (order.brotherId == _.id)
          if (parentContracted filterNot isSTOP_LIMIT).nonEmpty then 
            nextOrders should contain (order.invalidateParentId)
          if (parentContracted filter isSTOP_LIMIT).nonEmpty then 
            nextOrders should not contain (order)
          if brotherContracted.nonEmpty then 
            nextOrders should not contain (order)
          if parentContracted.isEmpty && brotherContracted.isEmpty then 
            nextOrders should contain (order)
      }
  }