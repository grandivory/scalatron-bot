package com.grandivory.scalatron.bot.util

import Math.abs

sealed trait PositionVector {
  def distance: Int
  def coordinate: Int
}

case class LeftRight(coordinate: Int) extends PositionVector {
  override val distance = abs(coordinate)

  def +(distance: Int): LeftRight = LeftRight(coordinate + distance)
  def -(distance: Int): LeftRight = LeftRight(coordinate - distance)

  def +(lr: LeftRight): LeftRight = LeftRight(coordinate + lr.coordinate)
  def -(lr: LeftRight): LeftRight = LeftRight(coordinate - lr.coordinate)

  def unary_- : LeftRight = LeftRight(-coordinate)

  override def toString: String = s"$coordinate"
}

case class UpDown(coordinate: Int) extends PositionVector {
  override val distance = abs(coordinate)

  def +(distance: Int): UpDown = UpDown(coordinate + distance)
  def -(distance: Int): UpDown = UpDown(coordinate - distance)

  def +(ud: UpDown): UpDown = UpDown(coordinate + ud.coordinate)
  def -(ud: UpDown): UpDown = UpDown(coordinate - ud.coordinate)

  def unary_- : UpDown = UpDown(-coordinate)

  override def toString: String = s"$coordinate"
}
