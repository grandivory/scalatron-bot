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
  def euclidianDistance: Double = Math.sqrt(x.distance*x.distance + y.distance*y.distance)

  def distanceTo(that: RelativePosition): Int = Math.max((x - that.x).distance, (y - that.y).distance)
  def euclidianDistanceTo(that: RelativePosition): Double = (that - this).euclidianDistance

  def direction: Option[Direction] = (x, y) match {
    case (LeftRight(0), UpDown(ud)) if ud < 0 => Some(Direction.Up)
    case (LeftRight(0), UpDown(ud)) if ud > 0 => Some(Direction.Down)
    case (LeftRight(lr), UpDown(0)) if lr < 0 => Some(Direction.Left)
    case (LeftRight(lr), UpDown(0)) if lr > 0 => Some(Direction.Right)
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

  def +(that: RelativePosition): RelativePosition = RelativePosition(x + that.x, y + that.y)
  def -(that: RelativePosition): RelativePosition = RelativePosition(x - that.x, y - that.y)

  override def compare(that: RelativePosition): Int = {
    if (distance < that.distance) -1
    else if (distance > that.distance) 1
    else 0
  }

  override def toString: String = s"$x:$y"
}

object RelativePosition {
  def parse(input: String) = {
    val (x: String, y: String) = input.splitAt(input.indexOf(':'))

    RelativePosition(x.toInt.right, y.drop(1).toInt.down)
  }
}

object Origin extends RelativePosition(LeftRight(0), UpDown(0))
