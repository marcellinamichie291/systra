package com.github.imomushi8.systra.util

case class ControlChart(center: Double, 
                        sigma: Double, 
                        prevX: Double,
                        prevBias: Int,
                        prevUpDown: Int,
                        prevScat: Int,
                        biasCount: Int, 
                        upDownCount: Int, 
                        scatCount: Int)

extension (prev: ControlChart)
  def plot(x: Double) = ControlChart.plot(x, prev)

object ControlChart:
  def init(center: Double, sigma: Double, initX: Double):ControlChart = 
    val centeredX = initX-center
    val initSign = if centeredX > 0 then 1 else -1
    val initScat = 
      if -sigma < centeredX && centeredX < sigma then 0
      else if -2*sigma < centeredX && centeredX <  2*sigma then 1
      else if -3*sigma < centeredX && centeredX <  3*sigma then 2
      else -1 // 管理線を超えたら異常
    ControlChart(center, sigma, initX, initSign, initSign, initScat, 1, 1, 1)

  def plot(x: Double, cc:ControlChart):ControlChart = cc match
    case ControlChart(center, sigma, prevX, prevBias, prevUpDown, prevScat, biasCount, upDownCount, scatCount) =>
      val centeredX = x - center

      /* 1. 中心線の片側に偏っている連続回数 */
      val (nextBias, nextBiasCount) = 
        if 0 < prevBias*centeredX && prevBias*centeredX < 3*sigma then (prevBias, biasCount+1)
        else (-prevBias, 0)
      
      /* 2. 上昇・下落の連続回数 */
      val (nextUpDown, nextUpDownCount) = 
        if 0 < prevUpDown*(x-prevX) then (prevUpDown, upDownCount+1)
        else (-prevUpDown, 0)

      /* 3. 変動をしている位置の連続回数 */
      val nextScat = 
        if -sigma < centeredX && centeredX < sigma then 0
        else if -2*sigma < centeredX && centeredX <  2*sigma then 1
        else if -3*sigma < centeredX && centeredX <  3*sigma then 2
        else -1 // 管理線を超えたら異常

      val nextScatCount = 
        if prevScat == nextScat then scatCount+1
        else 0

      ControlChart(center, sigma, x, nextBias, nextUpDown, nextScat, nextBiasCount, nextUpDownCount, nextScatCount)