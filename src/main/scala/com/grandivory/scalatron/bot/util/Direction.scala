package com.grandivory.scalatron.bot.util

sealed trait Direction {
  override def toString: String = this match {
    case Up => "0:-1"
    case UpRight => "1:-1"
    case Right => "1:0"
    case DownRight => "1:1"
    case Down => "0:1"
    case DownLeft => "-1:1"
    case Left => "-1:0"
    case UpLeft => "-1:-1"
  }
}

case object Up extends Direction
case object UpRight extends Direction
case object Right extends Direction
case object DownRight extends Direction
case object Down extends Direction
case object DownLeft extends Direction
case object Left extends Direction
case object UpLeft extends Direction
