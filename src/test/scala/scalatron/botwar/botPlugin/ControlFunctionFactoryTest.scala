package scalatron.botwar.botPlugin

import com.grandivory.scalatron.bot.commands.{ControlOpCode, Goodbye, React, Welcome}
import org.scalatest.{FunSpec, PrivateMethodTester}

class ControlFunctionFactoryTest extends FunSpec with PrivateMethodTester {
  describe("parseControlCode") {
    val parseControlCode = PrivateMethod[ControlOpCode]('parseControlCode)

    it("successfully parses a minimal welcome op code") {
      val result = ControlFunctionFactory invokePrivate
        parseControlCode("Welcome(name=Foo,apocalypse=5000,round=1,maxslaves=10")

      assertResult(Welcome(name = "Foo", numSimulationRounds = 5000, currentRound = 1, maxSlaves = 10))(result)
    }

    it("successfully parses a minimal goodbye op code") {
      val result = ControlFunctionFactory invokePrivate
        parseControlCode("Goodbye(energy=200)")

      assertResult(Goodbye(energy = 200))(result)
    }

    it("successfully parses a a minimal react op code") {
      val result = ControlFunctionFactory invokePrivate
        parseControlCode("React(generation=0,name=MattBot,time=100,view=?????WWWWWW_M_WSmsPpBbBbB,energy=100)")

//      val expectedView = ???

//      assertResult(React(generation = 0, name = "MattBot", currentRound = 100, view = ???, currentEnergy = 100))(result)
    }

    it("can handle extra parameters") {}
  }
}
