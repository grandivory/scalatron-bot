package com.grandivory

import spire.math.UByte

import scala.language.implicitConversions

package object util {
  implicit def int2Ubyte(i: Int): UByte = UByte(i)
}
