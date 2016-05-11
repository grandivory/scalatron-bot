package com.grandivory.scalatron.bot.util

import PositionVectorConversions._
import RelativePositionConversions._

import org.scalatest.FunSpec

class RelativePositionTest extends FunSpec {
  describe("distanceTo") {
    it("should accurately give the number of moves required to get between two positions, assuming no obstacles") {
      val fiveMoves = (3.left, 4.up) distanceTo (2.right, 2.up)
      val threeMoves = (2.right, 2.up) distanceTo (2.right, 1.down)
      val noMoves = (1.right, 1.up) distanceTo (1.right, 1.up)
      val farOut = Origin distanceTo (8.right, 16.up)

      assertResult(5)(fiveMoves)
      assertResult(3)(threeMoves)
      assertResult(0)(noMoves)
      assertResult(16)(farOut)
    }
  }

  describe("direction") {
    it("should return a cardinal direction only if it points directly in that direction") {
      val straightUp: RelativePosition = 7.up
      val straightRight: RelativePosition = 2.right
      val straightDown: RelativePosition = 1.down
      val straightLeft: RelativePosition = 4.left
      val upRight: RelativePosition = (2.up, 18.right)
      val downRight: RelativePosition = (1.down, 1.right)
      val downLeft: RelativePosition = (5.down, 9.left)
      val upLeft: RelativePosition = (3.up, 8.left)

      assertResult(Some(Direction.Up))(straightUp.direction)
      assertResult(Some(Direction.Right))(straightRight.direction)
      assertResult(Some(Direction.Down))(straightDown.direction)
      assertResult(Some(Direction.Left))(straightLeft.direction)
      assertResult(Some(Direction.UpRight))(upRight.direction)
      assertResult(Some(Direction.DownRight))(downRight.direction)
      assertResult(Some(Direction.DownLeft))(downLeft.direction)
      assertResult(Some(Direction.UpLeft))(upLeft.direction)
    }

    it("should return nothing if the relative position points nowhere") {
      assertResult(None)(Origin.direction)
    }
  }

  describe("The + method") {
    it("should return a new RelativePosition based off of moving one cell in a direction") {
      assertResult(RelativePosition(2.left, 2.down))(RelativePosition(2.left, 3.down) + Direction.Up)
      assertResult(RelativePosition(1.right, 1.up))(Origin + Direction.UpRight)
      assertResult(RelativePosition(2.left, 8.down))(RelativePosition(3.left, 8.down) + Direction.Right)
      assertResult(Origin)(RelativePosition(1.left, 1.up) + Direction.DownRight)
      assertResult(RelativePosition(3.left, 1.up))(RelativePosition(3.left, 2.up) + Direction.Down)
      assertResult(RelativePosition(8.right, 2.up))(RelativePosition(9.right, 3.up) + Direction.DownLeft)
      assertResult(RelativePosition(5.right, 2.up))(RelativePosition(6.right, 2.up) + Direction.Left)
      assertResult(RelativePosition(2.left, 8.up))(RelativePosition(1.left, 7.up) + Direction.UpLeft)
    }

    it("should be able to add two RelativePositions together to get a new vector") {
      // Test cases are of the form (`this`, `that`, `result`)
      val testCases: List[(RelativePosition, RelativePosition, RelativePosition)] = List(
        (Origin, RelativePosition(2.left, 2.up), RelativePosition(2.left, 2.up)),
        (RelativePosition(2.left, 1.up), RelativePosition(4.left, UpDown(0)), RelativePosition(6.left, 1.up)),
        (RelativePosition(2.left, 4.down), RelativePosition(2.left, 2.up), RelativePosition(4.left, 2.down)),
        (RelativePosition(1.right, 1.up), RelativePosition(1.right, 1.up), RelativePosition(2.right, 2.up))
      )

      testCases foreach {
        case (start, end, expectedResult) =>
          assertResult(expectedResult)(start + end)
      }
    }
  }

  describe("The - method") {
    it("should return a new RelativePosition that points from `that` to `this`") {
      // Test cases are of the form (`this`, `that`, `result`)
      val testCases: List[(RelativePosition, RelativePosition, RelativePosition)] = List(
        (Origin, RelativePosition(2.left, 2.up), RelativePosition(2.right, 2.down)),
        (RelativePosition(2.left, 1.up), RelativePosition(4.left, UpDown(0)), RelativePosition(2.right, 1.up)),
        (RelativePosition(2.left, 4.down), RelativePosition(2.left, 2.up), RelativePosition(LeftRight(0), 6.down)),
        (RelativePosition(1.right, 1.up), RelativePosition(1.right, 1.up), Origin)
      )

      testCases foreach {
        case (start, end, expectedResult) =>
          assertResult(expectedResult)(start - end)
      }
    }
  }
}
