package com.grandivory.scalatron.bot.util

import PositionVectorConversions._
import com.grandivory.scalatron.bot.util.Direction._

/**
  * Represents a cell position relative to the bot
  *
  * @param x The left/right position
  * @param y The up/down position
  */
case class RelativePosition(x: LeftRight = 0.left, y: UpDown = 0.up) extends Ordered[RelativePosition] {
  def distance: Int = Math.max(x.distance, y.distance) // Number of moves it would take to get to that spot

  def distanceTo(that: RelativePosition): Int = Math.max((x - that.x).distance, (y - that.y).distance)

  def direction: Option[Direction] = (x, y) match {
    case (LeftRight(0), UpDown(ud)) if ud < 0 => Some(Direction.Up)
    case (LeftRight(0), UpDown(ud)) => Some(Direction.Down)
    case (LeftRight(lr), UpDown(0)) if lr < 0 => Some(Direction.Left)
    case (LeftRight(lr), UpDown(0)) => Some(Direction.Right)
    case (LeftRight(lr), UpDown(ud)) if lr < 0 && ud < 0 => Some(Direction.UpLeft)
    case (LeftRight(lr), UpDown(ud)) if lr < 0 && ud > 0 => Some(Direction.DownLeft)
    case (LeftRight(lr), UpDown(ud)) if lr > 0 && ud < 0 => Some(Direction.UpRight)
    case (LeftRight(lr), UpDown(ud)) if lr > 0 && ud > 0 => Some(Direction.DownRight)
    case _ => None
  }

  def +(direction: Direction): RelativePosition = direction match {
    case Up => RelativePosition(x, y - 1)
    case UpRight => RelativePosition(x + 1, y - 1)
    case Direction.Right => RelativePosition(x + 1, y)
    case DownRight => RelativePosition(x + 1, y + 1)
    case Down => RelativePosition(x, y + 1)
    case DownLeft => RelativePosition(x - 1, y + 1)
    case Left => RelativePosition(x - 1, y)
    case UpLeft => RelativePosition(x - 1, y - 1)
  }

  override def compare(that: RelativePosition): Int = {
    if (distance < that.distance) -1
    else if (distance > that.distance) 1
    else 0
  }

  override def toString: String = s"$x:$y"
}

object Origin extends RelativePosition(0.left, 0.up)
