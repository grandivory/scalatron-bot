package com.grandivory.scalatron.bot.util

import PositionVectorConversions._
import RelativePositionConversions._
import ViewObject.Empty
import org.scalatest.{FunSpec, PrivateMethodTester}

class ViewTest extends FunSpec {
  trait ViewTester {
    val testView = View("PbbBM____")
  }

  describe("canSeeToxifera") {
    it("should be true if there are any toxifera") {
      val viewWithToxifera = View("PB_bM_b_p")

      val viewWithoutToxifera = View("PB_bM_b__")

      assertResult(true)(viewWithToxifera.canSeeToxifera)
      assertResult(false)(viewWithoutToxifera.canSeeToxifera)
    }
  }

  describe("objectAt") {
    it("should return the object at a given relative position") {
      new ViewTester {
        assertResult(Some(Zugar))(testView.objectAt((1.left, 1.up)))
        assertResult(Some(MasterBot))(testView.objectAt((0.right, 0.down)))
        assertResult(Some(Empty))(testView.objectAt((1.right, 0.down)))
      }
    }

    it("should fail gracefully if the position is not in view") {
      new ViewTester {
        assertResult(None)(testView.objectAt((2.right, 0.down)))
      }
    }
  }

  describe("nearestObject") {
    it("should return the closest object to the bot, excluding the bot itself") {
      val testView = View("_pb_W__B_sW_M_PW___W?WWW?")

      assertResult(Some((RelativePosition(0.right, 1.up), Fluppet)))(testView.nearestGameObject)
    }

    it("should return nothing if there is nothing in view") {
      val testView = View("____M____")

      assertResult(None)(testView.nearestGameObject)
    }
  }

  describe("canSeeZugars") {
    it("should be true if there are any zugars in view") {
      val viewWithZugars = View("PbBbM___p")

      val viewWithoutZugars = View("pbBbM___p")

      assertResult(true)(viewWithZugars.canSeeZugars)
      assertResult(false)(viewWithoutZugars.canSeeZugars)
    }
  }

  describe("canSeeFluppets") {
    it("should be true if there are any fluppets in view") {
      val viewWithFluppets = View("PB_bM_b_p")

      val viewWithoutFluppets = View("Pb_bM_b_p")

      assertResult(true)(viewWithFluppets.canSeeFluppets)
      assertResult(false)(viewWithoutFluppets.canSeeFluppets)

    }
  }

  describe("canSeeSnorgs") {
    it("should be true if there are any snorgs in view") {
      val viewWithSnorgs = View("PB_bM__bp")

      val viewWithoutSnorgs = View("PB_sM_s__")

      assertResult(true)(viewWithSnorgs.canSeeSnorgs)
      assertResult(false)(viewWithoutSnorgs.canSeeSnorgs)
    }
  }

  describe("canSee") {
    it("should be true if the space is visible") {
      def testView = View("WWWWMWWWW")

      assertResult(true)(testView.canSee((1.right, 1.down)))
      assertResult(false)(testView.canSee((2.right, 1.down)))
      assertResult(false)(testView.canSee((123.right, 8.down)))
    }
  }

  describe("apply") {
    it("should be able to parse a valid view string") {
      new ViewTester {
        assertResult(Some(Zugar))(testView.objectAt((1.left, 1.up)))
        assertResult(Some(Snorg))(testView.objectAt(1.up))
        assertResult(Some(Snorg))(testView.objectAt((1.right, 1.up)))
        assertResult(Some(Fluppet))(testView.objectAt(1.left))
        assertResult(Some(MasterBot))(testView.objectAt(Origin))
        assertResult(Some(Empty))(testView.objectAt(1.right))
        assertResult(Some(Empty))(testView.objectAt((1.left, 1.down)))
        assertResult(Some(Empty))(testView.objectAt(1.down))
        assertResult(Some(Empty))(testView.objectAt((1.right, 1.down)))

        assertResult(1)(testView.distance)
      }
    }

    it("should properly handle different-sized strings") {
      val tinyView = View("M")

      assertResult(0)(tinyView.distance)
      assertResult(Some(MasterBot))(tinyView.objectAt((0.left, 0.up)))

      val bigView = View("?????WWWWWBbMmSPpPpP___s_")
      assertResult(2)(bigView.distance)
      assertResult(None)(bigView.objectAt((2.up, 2.left)))
      assertResult(None)(bigView.objectAt((2.up, 1.left)))
      assertResult(None)(bigView.objectAt(2.up))
      assertResult(None)(bigView.objectAt((2.up, 1.right)))
      assertResult(None)(bigView.objectAt((2.up, 2.right)))
      assertResult(Some(Wall))(bigView.objectAt((1.up, 2.left)))
      assertResult(Some(Wall))(bigView.objectAt((1.up, 1.left)))
      assertResult(Some(Wall))(bigView.objectAt(1.up))
      assertResult(Some(Wall))(bigView.objectAt((1.up, 1.right)))
      assertResult(Some(Wall))(bigView.objectAt((1.up, 2.right)))
      assertResult(Some(Fluppet))(bigView.objectAt(2.left))
      assertResult(Some(Snorg))(bigView.objectAt(1.left))
      assertResult(Some(MasterBot))(bigView.objectAt(Origin))
      assertResult(Some(EnemyBot))(bigView.objectAt(1.right))
      assertResult(Some(Slave))(bigView.objectAt(2.right))
      assertResult(Some(Zugar))(bigView.objectAt((1.down, 2.left)))
      assertResult(Some(Toxifera))(bigView.objectAt((1.down, 1.left)))
      assertResult(Some(Zugar))(bigView.objectAt(1.down))
      assertResult(Some(Toxifera))(bigView.objectAt((1.down, 1.right)))
      assertResult(Some(Zugar))(bigView.objectAt((1.down, 2.right)))
      assertResult(Some(Empty))(bigView.objectAt((2.down, 2.left)))
      assertResult(Some(Empty))(bigView.objectAt((2.down, 1.left)))
      assertResult(Some(Empty))(bigView.objectAt(2.down))
      assertResult(Some(EnemySlave))(bigView.objectAt((2.down, 1.right)))
      assertResult(Some(Empty))(bigView.objectAt((2.down, 2.right)))
    }
  }

  describe("nearest") {
    it("should return the nearest object that matches a predicate") {
      val tinyView = View("b___M____")
      val biggerView = View("pp__WmmssS__M_BppPWWBPPbs")

      assertResult(Some(RelativePosition(1.left, 1.up)))(tinyView.nearest(Snorg == _))
      assertResult(Some(RelativePosition(1.left, 1.up)))(biggerView.nearest(EnemyBot == _))
    }

    it("should return none if the predicate is not met") {
      val view = View("pp__WmmssS__M_BppPWWBPPBs")

      assertResult(None)(view.nearest(Snorg == _))
    }
  }
}
