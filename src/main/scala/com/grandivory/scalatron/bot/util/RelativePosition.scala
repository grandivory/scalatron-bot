package com.grandivory.scalatron.bot.util

import PositionVectorConversions._

/**
  * Represents a cell position relative to the bot
  *
  * @param x The left/right position
  * @param y The up/down position
  */
case class RelativePosition(x: LeftRight = 0.left, y: UpDown = 0.up) extends Ordered[RelativePosition] {
  import Math._

  def distance: Int = max(x.distance, y.distance) // Number of moves it would take to get to that spot

  override def compare(that: RelativePosition): Int = {
    if (distance < that.distance) -1
    else if (distance > that.distance) 1
    else 0
  }

  override def toString: String = s"$x:$y"
}

object Origin extends RelativePosition(0.left, 0.up)
