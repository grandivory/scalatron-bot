package com.grandivory.scalatron.bot.util

import spire.math.UByte

case class Color(red: UByte, green: UByte, blue: UByte) {
  override def toString: String = f"#${red.intValue}%02x${green.intValue}%02x${blue.intValue}%02x"
}
