package com.grandivory.scalatron.bot.util

import PositionVectorConversions._
import org.scalatest.FunSpec

class PositionVectorConversionsTest extends FunSpec {
  describe("left and right") {
    it("should provide equality between directions if the sign changes") {
      assertResult(4.right)((-4).left)
      assertResult(6.right)(-6.left)
      assertResult(7.left)((-7).right)
      assertResult(2.left)(-2.right)
    }

    it("should always be the same for 0") {
      assertResult(0.left)(0.right)
    }
  }

  describe("up and down") {
    it("should provide equality between directions if the sign changes") {
      assertResult(4.down)((-4).up)
      assertResult(6.down)(-6.up)
      assertResult(7.up)((-7).down)
      assertResult(2.up)(-2.down)
    }

    it("should always be the same for 0") {
      assertResult(0.up)(0.down)
    }
  }
}
