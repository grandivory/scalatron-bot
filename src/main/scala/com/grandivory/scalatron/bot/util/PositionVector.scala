package com.grandivory.scalatron.bot.util

import Math.abs

sealed trait PositionVector {
  def distance: Int
  def coordinate: Int
}

case class LeftRight(coordinate: Int) extends PositionVector {
  override val distance = abs(coordinate)

  def unary_- : LeftRight = LeftRight(-coordinate)

  override def toString: String = s"$coordinate"
}

case class UpDown(coordinate: Int) extends PositionVector {
  override val distance = abs(coordinate)

  def unary_- : UpDown = UpDown(-coordinate)

  override def toString: String = s"$coordinate"
}
