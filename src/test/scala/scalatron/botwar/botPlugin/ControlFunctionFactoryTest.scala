package scalatron.botwar.botPlugin

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util._
import RelativePositionConversions._
import PositionVectorConversions._
import org.scalatest.{FunSpec, PrivateMethodTester}

class ControlFunctionFactoryTest extends FunSpec with PrivateMethodTester {
  ignore("parseControlCode") {
    val privateMethod = PrivateMethod[ControlOpCode]('parseControlCode)

    def parseControlCode(input: String): ControlOpCode = new ControlFunctionFactory invokePrivate privateMethod(input)

    it("successfully parses a minimal welcome op code") {
      val result = parseControlCode("Welcome(name=Foo,apocalypse=5000,round=1,maxslaves=10")

      assertResult(Welcome(name = "Foo", numSimulationRounds = 5000, currentRound = 1, maxSlaves = 10))(result)
    }

    it("successfully parses a minimal goodbye op code") {
      val result = parseControlCode("Goodbye(energy=200)")

      assertResult(Goodbye(energy = 200))(result)
    }

    it("successfully parses a a minimal react op code") {
      val result = parseControlCode("React(generation=0,name=MattBot,time=100,view=?????WWWWWW_M_WSmsPpBbBbB,energy=100)")

//      val expectedView = ???

//      assertResult(React(generation = 0, name = "MattBot", currentRound = 100, view = ???, currentEnergy = 100))(result)
    }

    it("can handle extra parameters") {}
  }

  describe("serializeBotAction") {
    val privateMethod = PrivateMethod[String]('serializeBotAction)

    def serializeBotAction(input: BotCommand): String = new ControlFunctionFactory invokePrivate privateMethod(input)

    it("can send a status code") {
      assertResult("Status(text=foo!)")(serializeBotAction(Status("foo!")))
    }

    it("can handle a move command in any direction") {
      def up = serializeBotAction(Move(Direction.Up))
      def upRight = serializeBotAction(Move(Direction.UpRight))
      def right = serializeBotAction(Move(Direction.Right))
      def downRight = serializeBotAction(Move(Direction.DownRight))
      def down = serializeBotAction(Move(Direction.Down))
      def downLeft = serializeBotAction(Move(Direction.DownLeft))
      def left = serializeBotAction(Move(Direction.Left))
      def upLeft = serializeBotAction(Move(Direction.UpLeft))

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
      def resultNoName = serializeBotAction(Spawn(Direction.UpLeft, None, 200, None))
      def resultWithNameAndProps = serializeBotAction(
        Spawn(Direction.Down, Some("foobar"), 250, Some(Map("foo" -> "bar", "your" -> "face")))
      )

      assertResult("Spawn(direction=-1:-1,energy=200)")(resultNoName)
      assertResult("Spawn(direction=0:1,name=foobar,energy=250,foo=bar,your=face)")(resultWithNameAndProps)
    }

    it("can draw a line") {
      def result = serializeBotAction(DrawLine((2.left, 2.down), 2.right, Color(255, 255, 255)))

      assertResult("DrawLine(from=-2:2,to=2:0,color=#ffffff)")(result)
    }

    it("can cause a bot to explode") {
      assertResult("Explode(size=6)")(serializeBotAction(Explode(6)))
    }

    it("can log a message") {
      assertResult("Log(text=foobar!)")(serializeBotAction(Log("foobar!")))
    }

    it("can mark a cell") {
      def result = serializeBotAction(MarkCell((3.up, 2.left), Color(128, 255, 4)))

      assertResult("MarkCell(position=-2:-3,color=#80ff04)")(result)
    }

    it("can do nothing") {
      assertResult("")(serializeBotAction(Noop))
    }

    it("can say something") {
      assertResult("Say(text=Your mother was a hamster)")(serializeBotAction(Say("Your mother was a hamster")))
    }

    it("can set arbitrary properties on the bot") {
      def oneProperty = serializeBotAction(SetProperties(Map("foo" -> "bar")))
      def multipleProperties = serializeBotAction(SetProperties(Map("foo" -> "bar", "baz" -> "qux")))

      assertResult("Set(foo=bar)")(oneProperty)
      assertResult("Set(foo=bar,baz=qux)")(multipleProperties)
    }

    it("can send multiple commands") {
      def moveAndSay = serializeBotAction(Move(Direction.Up) + Say("foobar!"))

      assertResult("Move(direction=0:-1)|Say(text=foobar!)")(moveAndSay)
    }
  }
}
