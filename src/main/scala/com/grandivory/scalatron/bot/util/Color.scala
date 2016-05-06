package com.grandivory.scalatron.bot.util

case class Color(red: Int, green: Int, blue: Int) {
  override def toString: String = f"#$red%02x$green%02x$blue%02x"
}
