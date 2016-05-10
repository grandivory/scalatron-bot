package com.grandivory.scalatron.bot.util

import PositionVectorConversions._
import RelativePositionConversions._

sealed trait Direction extends Product with Serializable {
  import Direction._

  def toRelativePosition: RelativePosition = this match {
    case Up => 1.up
    case UpRight => (1.right, 1.up)
    case Right => 1.right
    case DownRight => (1.right, 1.down)
    case Down => 1.down
    case DownLeft => (1.left, 1.down)
    case Left => 1.left
    case UpLeft => (1.left, 1.up)
  }

  def right45: Direction = this match {
    case Up => UpRight
    case UpRight => Right
    case Right => DownRight
    case DownRight => Down
    case Down => DownLeft
    case DownLeft => Left
    case Left => UpLeft
    case UpLeft => Up
  }

  def left45: Direction = this match {
    case Up => UpLeft
    case UpRight => Up
    case Right => UpRight
    case DownRight => Right
    case Down => DownRight
    case DownLeft => Down
    case Left => DownLeft
    case UpLeft => Left
  }

  def right90: Direction = this match {
    case Up => Right
    case UpRight => DownRight
    case Right => Down
    case DownRight => DownLeft
    case Down => Left
    case DownLeft => UpLeft
    case Left => Up
    case UpLeft => UpRight
  }

  def left90: Direction = this match {
    case Up => Left
    case UpRight => UpLeft
    case Right => Up
    case DownRight => UpRight
    case Down => Right
    case DownLeft => DownRight
    case Left => Down
    case UpLeft => DownLeft
  }

  def reverse: Direction = this match {
    case Up => Down
    case UpRight => DownLeft
    case Right => Left
    case DownRight => UpLeft
    case Down => Up
    case DownLeft => UpRight
    case Left => Right
    case UpLeft => DownRight
  }

  def *(distance: Int): RelativePosition = this match {
    case Up => distance.up
    case UpRight => (distance.up, distance.right)
    case Right => distance.right
    case DownRight => (distance.down, distance.right)
    case Down => distance.down
    case DownLeft => (distance.down, distance.left)
    case Left => distance.left
    case UpLeft => (distance.up, distance.left)
  }

  override def toString: String = directionStringMap(this)
}

object Direction {
  private val directionStringMap: Map[Direction, String] = Map(
    Up -> "0:-1",
    UpRight -> "1:-1",
    Right -> "1:0",
    DownRight -> "1:1",
    Down -> "0:1",
    DownLeft -> "-1:1",
    Left -> "-1:0",
    UpLeft -> "-1:-1"
  )

  def parse(input: String): Direction = directionStringMap.map(_.swap).apply(input)

  case object Up extends Direction
  case object UpRight extends Direction
  case object Right extends Direction
  case object DownRight extends Direction
  case object Down extends Direction
  case object DownLeft extends Direction
  case object Left extends Direction
  case object UpLeft extends Direction

  val allPossibleDirections: Set[Direction] = Set(
    Up,
    UpRight,
    Right,
    DownRight,
    Down,
    DownLeft,
    Left,
    UpLeft
  )
}
