package scalatron.botwar.botPlugin

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util._
import org.scalatest.{FunSpec, PrivateMethodTester}

class ControlFunctionFactoryTest extends FunSpec with PrivateMethodTester {
  describe("parseControlCode") {
    val parseControlCode = PrivateMethod[ControlOpCode]('parseControlCode)

    def getResult(input: String): ControlOpCode = new ControlFunctionFactory invokePrivate parseControlCode(input)

    it("successfully parses a minimal welcome op code") {
      val result = getResult("Welcome(name=Foo,apocalypse=5000,round=1,maxslaves=10")

      assertResult(Welcome(name = "Foo", numSimulationRounds = 5000, currentRound = 1, maxSlaves = 10))(result)
    }

    it("successfully parses a minimal goodbye op code") {
      val result = getResult("Goodbye(energy=200)")

      assertResult(Goodbye(energy = 200))(result)
    }

    it("successfully parses a a minimal react op code") {
      val result = getResult("React(generation=0,name=MattBot,time=100,view=?????WWWWWW_M_WSmsPpBbBbB,energy=100)")

//      val expectedView = ???

//      assertResult(React(generation = 0, name = "MattBot", currentRound = 100, view = ???, currentEnergy = 100))(result)
    }

    it("can handle extra parameters") {}
  }

  describe("serializeBotAction") {
    val serializeBotAction = PrivateMethod[String]('serializeBotAction)

    def getResult(input: BotCommand): String = new ControlFunctionFactory invokePrivate serializeBotAction(input)

    it("can send a status code") {
      val result = getResult(Status("foo!"))

      assertResult("Status(text=foo!)")(result)
    }

    it("can handle a move command in any direction") {
      def up = getResult(Move(Up))
      def upRight = getResult(Move(UpRight))
      def right = getResult(Move(Right))
      def downRight = getResult(Move(DownRight))
      def down = getResult(Move(Down))
      def downLeft = getResult(Move(DownLeft))
      def left = getResult(Move(Left))
      def upLeft = getResult(Move(UpLeft))

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
      def resultNoName = getResult(Spawn(UpLeft, None, 200, None))
      def resultWithNameAndProps = getResult(Spawn(Down, Some("foobar"), 250, Some(Map("foo" -> "bar", "your" -> "face"))))

      assertResult("Spawn(direction=-1:-1,energy=200")(resultNoName)
      assertResult("Spawn(direction=0:1,name=foobar,energy=250,foo=bar,your=face")(resultWithNameAndProps)
    }
  }
}
