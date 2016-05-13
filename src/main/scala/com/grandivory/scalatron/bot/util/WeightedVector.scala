package com.grandivory.scalatron.bot.util

import PositionVectorConversions._

case class WeightedVector(x: Float, y: Float) extends Ordered[WeightedVector] {
  val scale: Double = Math.sqrt(x*x + y*y)

  def +(that: WeightedVector): WeightedVector = WeightedVector(x + that.x, y + that.y)
  def -(that: WeightedVector): WeightedVector = WeightedVector(x - that.x, y - that.y)

  def toRelativePosition: RelativePosition = RelativePosition(Math.round(x).right, Math.round(y).down)

  override def compare(that: WeightedVector): Int = {
    if (scale < that.scale) -1
    else if (scale > that.scale) 1
    else 0
  }
}
