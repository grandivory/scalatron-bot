package com.grandivory.scalatron.bot.util

import scala.language.implicitConversions

object RelativePositionConversions {
  implicit def xy2RelativePosition(xy: (LeftRight, UpDown)): RelativePosition = RelativePosition(xy._1, xy._2)
  implicit def yx2RelativePosition(yx: (UpDown, LeftRight)): RelativePosition = RelativePosition(yx._2, yx._1)
  implicit def x2RelativePosition(x: LeftRight): RelativePosition = RelativePosition(x, new UpDown(0))
  implicit def y2RelativePosition(y: UpDown): RelativePosition = RelativePosition(new LeftRight(0), y)
}
