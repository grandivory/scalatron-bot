package com.grandivory.scalatron.bot.util

sealed trait Direction {
  import Direction._

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
}
