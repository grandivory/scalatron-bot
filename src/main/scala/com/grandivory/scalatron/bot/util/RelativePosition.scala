package com.grandivory.scalatron.bot.util

/**
  * Represents a cell position relative to the bot
  * @param x
  * @param y
  */
case class RelativePosition(x: Int = 0, y: Int = 0) extends Ordered[RelativePosition] {

  def distance: Double = Math.sqrt(x*x + y*y)

  override def compare(that: RelativePosition): Int = {
    if (distance < that.distance) -1
    else if (distance > that.distance) 1
    else 0
  }

  override def toString: String = s"$x:$y"
}
