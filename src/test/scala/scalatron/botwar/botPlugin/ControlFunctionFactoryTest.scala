package scalatron.botwar.botPlugin

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util._
import RelativePositionConversions._
import PositionVectorConversions._
import com.grandivory.scalatron.bot.util.Direction.DownRight
import org.scalatest.{FunSpec, PrivateMethodTester}

class ControlFunctionFactoryTest extends FunSpec with PrivateMethodTester {
  describe("parseControlCode") {
    val privateMethod = PrivateMethod[Option[ControlOpCode]]('parseControlCode)

    def parseControlCode(input: String): Option[ControlOpCode] =
      new ControlFunctionFactory invokePrivate privateMethod(input)

    it("successfully parses a minimal welcome op code") {
      val result = parseControlCode("Welcome(name=Foo,apocalypse=5000,round=1,maxslaves=10)")

      assertResult(Some(Welcome(name = "Foo", numSimulationRounds = 5000, currentRound = 1, maxSlaves = 10)))(result)
    }

    it("successfully parses a minimal goodbye op code") {
      val result = parseControlCode("Goodbye(energy=200)")

      assertResult(Some(Goodbye(energy = 200)))(result)
    }

    it("successfully parses a a minimal react op code") {
      val result = parseControlCode(
        "React(generation=0,name=foobar,time=100,view=?????WWWWWW_M_WSmsPpBbBbB,energy=100,slaves=0)"
      )

      assertResult(
        Some(React(
          generation = 0,
          name = "foobar",
          currentRound = 100,
          view = View("?????WWWWWW_M_WSmsPpBbBbB"),
          currentEnergy = 100,
          numLivingSlaves = 0
        ))
      )(result)
    }

    it("can handle extra parameters") {
      val result = parseControlCode(
        "React(generation=1,name=foobar,time=4,view=M,energy=100,master=1:1,collision=1:1,slaves=2,foo=bar)"
      )

      assertResult(
        Some(React(
          generation = 1,
          name = "foobar",
          currentRound = 4,
          view = View("M"),
          currentEnergy = 100,
          masterPosition = Some(RelativePosition(1.right, 1.down)),
          failedMoveDirection = Some(DownRight),
          numLivingSlaves = 2,
          extraProperties = Some(Map("foo" -> "bar"))
        ))
      )(result)
    }
  }

  describe("serializeBotAction") {
    val privateMethod = PrivateMethod[String]('serializeBotAction)

    def serializeBotAction(input: Option[BotCommand]): String =
      new ControlFunctionFactory invokePrivate privateMethod(input)

    it("can send a status code") {
      assertResult("Status(text=foo!)")(serializeBotAction(Some(Status("foo!"))))
    }

    it("can handle a move command in any direction") {
      def up = serializeBotAction(Some(Move(Direction.Up)))
      def upRight = serializeBotAction(Some(Move(Direction.UpRight)))
      def right = serializeBotAction(Some(Move(Direction.Right)))
      def downRight = serializeBotAction(Some(Move(Direction.DownRight)))
      def down = serializeBotAction(Some(Move(Direction.Down)))
      def downLeft = serializeBotAction(Some(Move(Direction.DownLeft)))
      def left = serializeBotAction(Some(Move(Direction.Left)))
      def upLeft = serializeBotAction(Some(Move(Direction.UpLeft)))

      assertResult("Move(direction=0:-1)")(up)
      assertResult("Move(direction=1:-1)")(upRight)
      assertResult("Move(direction=1:0)")(right)
      assertResult("Move(direction=1:1)")(downRight)
      assertResult("Move(direction=0:1)")(down)
      assertResult("Move(direction=-1:1)")(downLeft)
      assertResult("Move(direction=-1:0)")(left)
      assertResult("Move(direction=-1:-1)")(upLeft)
    }

    it("can spawn a new slave bot") {
      def resultNoName = serializeBotAction(Some(Spawn(Direction.UpLeft, None, 200, None)))
      def resultWithNameAndProps = serializeBotAction(
        Some(Spawn(Direction.Down, Some("foobar"), 250, Some(Map("foo" -> "bar", "your" -> "face"))))
      )

      assertResult("Spawn(direction=-1:-1,energy=200)")(resultNoName)
      assertResult("Spawn(direction=0:1,name=foobar,energy=250,foo=bar,your=face)")(resultWithNameAndProps)
    }

    it("can draw a line") {
      def result = serializeBotAction(Some(DrawLine((2.left, 2.down), 2.right, Color(255, 255, 255))))

      assertResult("DrawLine(from=-2:2,to=2:0,color=#ffffff)")(result)
    }

    it("can cause a bot to explode") {
      assertResult("Explode(size=6)")(serializeBotAction(Some(Explode(6))))
    }

    it("can log a message") {
      assertResult("Log(text=foobar!)")(serializeBotAction(Some(Log("foobar!"))))
    }

    it("can mark a cell") {
      def result = serializeBotAction(Some(MarkCell((3.up, 2.left), Color(128, 255, 4))))

      assertResult("MarkCell(position=-2:-3,color=#80ff04)")(result)
    }

    it("can do nothing") {
      assertResult("")(serializeBotAction(None))
    }

    it("can say something") {
      assertResult("Say(text=Your mother was a hamster)")(serializeBotAction(Some(Say("Your mother was a hamster"))))
    }

    it("can set arbitrary properties on the bot") {
      def oneProperty = serializeBotAction(Some(SetProperties(Map("foo" -> "bar"))))
      def multipleProperties = serializeBotAction(Some(SetProperties(Map("foo" -> "bar", "baz" -> "qux"))))

      assertResult("Set(foo=bar)")(oneProperty)
      assertResult("Set(foo=bar,baz=qux)")(multipleProperties)
    }

    it("can send multiple commands") {
      def moveAndSay = serializeBotAction(Some(Move(Direction.Up) + Say("foobar!")))

      assertResult("Move(direction=0:-1)|Say(text=foobar!)")(moveAndSay)
    }
  }
}
