package com.grandivory.scalatron.bot.util

/**
  * Represents a cell position relative to the bot
  * @param x
  * @param y
  */
case class RelativePosition(x: Int = 0, y: Int = 0) {
  override def toString: String = s"$x:$y"
}
