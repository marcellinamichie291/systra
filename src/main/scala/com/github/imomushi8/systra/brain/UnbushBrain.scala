package com.github.imomushi8.systra.brain

import cats._
import cats.data.{StateT}
import cats.implicits.catsSyntaxFlatMapOps
import cats.effect.IO
import cats.implicits.catsSyntaxOptionId

import com.github.imomushi8.systra._
import com.github.imomushi8.systra.util._
import com.github.imomushi8.systra.entity._
import com.github.imomushi8.systra.Actions._
import com.github.imomushi8.systra.behavior._
import org.atnos.eff.memo

inline val tradeRisk = 0.02
inline val maxLoss = -10000.0
inline val usdjpyPips = 0.01
inline val lot = 1000

/*
トレード毎のリスク：2%
ロスカット価格：10000円
pips：0.01円
1ロット：1000通貨
*/

/**
 * 高値安値待ち伏せ + 資金管理の戦略
*/
object UnbushBrain:
  case class Memory(chartList: List[Chart])

  given Monoid[Memory] with
    override def empty: Memory = Memory(Nil)
    override def combine(x: Memory, y: Memory): Memory = Memory(
      x.chartList ++ y.chartList
    )

  def apply[Market](previousDay: Int)
                   (using MarketBehavior[Market]): Brain[Market, Memory] = Actions.receive {
    (chart, context, memory) =>
      val nextChartList =
        if (memory.chartList.size > previousDay) chart +: memory.chartList.take(previousDay)
        else chart +: memory.chartList
      val expire = chart.datetime.plusMonths(6)
      
      // 追跡中の注文がない場合
      if context.positions.isEmpty && context.orders.isEmpty then
        val riskCapital = tradeRisk*context.capital
        val (method, size) = 
          if chart.close - nextChartList.last.close > 0 then 
            (STOP(BUY, chart.high), riskCapital/chart.high)
          else 
            (STOP(SELL, chart.low), riskCapital/chart.low)
        
        val ref = for
          refMarket <- context.getMarket
          _ <- refMarket.placeOrder(method, size, expire)
        yield 
          refMarket
        Actions.nextHandleErrorWith(Memory(nextChartList), ref) //シグナルに従って待ち伏せ注文
      
      // 追跡中の注文が
      else
        // 約定している場合
        if context.positions.nonEmpty then
          val position = context.positions.head
          val ref = for
            refMarket <- context.getMarket
            _<-refMarket.placeOrder(MARKET(position.oppositeSide, position.id), position.size, expire)
          yield refMarket
          Actions.nextHandleErrorWith(Memory(nextChartList), ref) // 決済する

        // 未約定の場合
        else // 何もしない
          Actions.next(Memory(nextChartList), context)
  }