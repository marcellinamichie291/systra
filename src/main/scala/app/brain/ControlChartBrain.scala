package app.brain

import cats._
import cats.data.{StateT}
import cats.implicits.catsSyntaxFlatMapOps
import cats.effect.IO
import cats.implicits.catsSyntaxOptionId

import breeze.linalg.{diff, DenseVector}
import breeze.numerics.{sqrt, pow, log, abs}
import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Rand.VariableSeed.randBasis

import com.github.imomushi8.systra.core.util._
import com.github.imomushi8.systra.core.entity._
import com.github.imomushi8.systra.core.action._
import com.github.imomushi8.systra.core.market._

import java.time.LocalDateTime

inline val minimumLot = 1000.0

val gauss = Gaussian(0, 1)

object ControlChartBrain:
  case class Memory(chartList: List[Chart], cc: Option[ControlChart], prevOHLCV: Chart, orderDT: TimeStamp)

  given Initial[Memory] with
    override def empty(): Memory = Memory(Nil, None, Chart(0,0,0,0,0,LocalDateTime.MIN),LocalDateTime.MIN)

  def apply[Market](expiredHour: Int=24,
                    maxBias: Int=25,
                    maxUpDown: Int=10,
                    maxScat: Int=50)
                   (using MarketBehavior[Market]): Brain[Market, Memory] = Actions.receive {
    (chart, context, memory) =>
      val nowChartList =
        if (memory.chartList.size > 59) chart +: memory.chartList.take(59)
        else chart +: memory.chartList
      val expire = chart.datetime.plusHours(expiredHour) // 期限切れ

      /* 前回のアクションから60分以内 */
      if chart.datetime.minusHours(1) isBefore memory.prevOHLCV.datetime then

        // ポジションがなければ何もしない
        if context.positions.isEmpty then
          Actions.next(memory.copy(chartList=nowChartList), context)

        // ポジションがあればポジション管理を行う
        else
          val position = context.positions.head
          // ここで管理図を更新していく
          val nowCC = memory.cc.orElse {
            val count = nowChartList.size
            val upStd   = math.sqrt(nowChartList.map { c => val tmp = c.high-c.close; tmp*tmp }.sum * count)
            val downStd = math.sqrt(nowChartList.map { c => val tmp = c.low -c.close; tmp*tmp }.sum * count)
            ControlChart.init(position.price, (upStd+downStd)/2, chart.close).some
            /*
            val ohlcv = memory.prevOHLCV
            ControlChart.init(position.price, (ohlcv.high-ohlcv.low)/6, chart.close).some // 前回がNoneであれば初期化
            */
          }.map(_.plot(chart.close))
          
          // 管理図で異常判定が起きた場合
          if nowCC.exists { cc => 
            cc.prevScat    < 0 || 
            cc.biasCount   > maxBias || 
            cc.upDownCount > maxUpDown || 
            cc.scatCount   > maxScat } then

            val ref = for
              refMarket <- context.getMarket
              _<-refMarket.placeOrder(MARKET(position.oppositeSide, position.id), position.size, expire)
            yield refMarket
            Actions.nextHandleErrorWith(memory.copy(chartList=nowChartList, cc=None), ref) // 決済する
          
          // 異常なしなら管理図を更新して次に回す
          else
            Actions.next(memory.copy(chartList=nowChartList, cc=nowCC), context)
          
      /* 60分の間隔で注文を行う */
      else
        val nowOHLCV = Chart(
          nowChartList.last.open,
          nowChartList.map(_.high).max,
          nowChartList.map(_.low).min,
          nowChartList.head.close,
          nowChartList.map(_.volume).sum,
          nowChartList.last.datetime)

        // 追跡中の注文が約定して存在している場合は強制的に約定させる
        if context.positions.nonEmpty then
          val position = context.positions.head
          val ref = for
            refMarket <- context.getMarket
            _ <- refMarket.placeOrder(MARKET(position.oppositeSide, position.id), position.size, expire)
          yield refMarket
          Actions.nextHandleErrorWith(memory.copy(nowChartList, None, nowOHLCV), ref) // 決済する

        // 追跡中の注文が約定せず存在している場合はなにもしない（ずっとそのままだといずれキャンセルされる）
        else if context.orders.nonEmpty then
          Actions.next(memory.copy(nowChartList, None, nowOHLCV), context)
        // 追跡中の注文が存在していない場合(未約定、管理線で異常判定、注文キャンセルの場合)は、待ち伏せトレード開始
        else
          // 現在位置から前回高値以上には確率x%, 安値以下にはy%で到達するので、x%(y%)の方が大きいため高値(安値)に待ち伏せする       
          val closes = log(DenseVector(nowChartList.map(_.close)*))
          val rates = diff(closes)
          val mv = breeze.stats.meanAndVariance(rates)
          val T = 60.0
          def x(target:Double) = (math.log(target/chart.close) - (mv.mean - mv.variance/2)*T)/(mv.stdDev*math.sqrt(T))
          
          // 高値以上の確率 - 安値以上の確率
          val trend = 1 - gauss.cdf(x(nowOHLCV.high)) - gauss.cdf(x(nowOHLCV.low))
          val (side, orderPrice) = if trend > 0 then (BUY, chart.high) else (SELL, chart.low) // trend方向に待ち伏せ
          
          if trend != 0 then
            val expect     = math.abs(nowOHLCV.close - memory.prevOHLCV.close)
            val volatility = (nowOHLCV.high max memory.prevOHLCV.high) - (memory.prevOHLCV.low max nowOHLCV.low)
            val optimalF = UtilTrade.optimalF(expect, volatility, 0.5)
            val size = UtilTrade.backwordSizeForCapital(if optimalF>=1 then 0.5 else optimalF, orderPrice, context.capital) max minimumLot

            (for // トレンド方向に待ち伏せ
              refMarket <- context.getMarket
              _ <- refMarket.placeOrder(STOP(side, orderPrice), size, expire)
            yield 
              refMarket).>>= { ref => 
              Actions.next(Memory(nowChartList, None, nowOHLCV, chart.datetime), context, ref.some)
            } handleError { t => 
              Actions.end(t.getMessage)
              //context.getMarket // 元の状態に戻す
            }

          // トレンド判定ができない（trend=0ということは高値に行く確率と安値に行く確率が等しい）
          else
            Actions.next(memory.copy(nowChartList, None, nowOHLCV), context) //なにもせず終了
  }