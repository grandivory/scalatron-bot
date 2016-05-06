package com.grandivory.scalatron.bot.util

object PositionVectorConversions {
  implicit class IntExtender(val underlying: Int) extends AnyVal {
    def left: LeftRight = LeftRight(-underlying)
    def right: LeftRight = LeftRight(underlying)
    def up: UpDown = UpDown(-underlying)
    def down: UpDown = UpDown(underlying)
  }
}
