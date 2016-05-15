package scalatron.botwar.botPlugin

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.Direction.DownRight
import com.grandivory.scalatron.bot.util.PositionVectorConversions._
import com.grandivory.scalatron.bot.util.RelativePositionConversions._
import com.grandivory.scalatron.bot.util._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, PrivateMethodTester}

import scala.util.{Success, Try}

class ControlFunctionFactoryTest extends FunSpec with GeneratorDrivenPropertyChecks with PrivateMethodTester {
  describe("parseControlCode") {
    val privateMethod = PrivateMethod[Either[ControlCodeParseException, ControlOpCode]]('parseControlCode)

    def parseControlCode(input: String): Either[ControlCodeParseException, ControlOpCode] =
      new ControlFunctionFactory invokePrivate privateMethod(input)

    it("successfully parses a minimal welcome op code") {
      val result = parseControlCode("Welcome(name=Foo,apocalypse=5000,round=1,maxslaves=10)")

      assertResult(Right(Welcome(name = "Foo", numSimulationRounds = 5000, currentRound = 1, maxSlaves = Some(10))))(result)
    }

    it("can handle maxSlaves missing") {
      val result = parseControlCode("Welcome(name=Foo,apocalypse=5000,round=8)")

      assertResult(Right(Welcome(name = "Foo", numSimulationRounds = 5000, currentRound = 8, maxSlaves = None)))(result)
    }

    it("successfully parses a minimal goodbye op code") {
      val result = parseControlCode("Goodbye(energy=200)")

      assertResult(Right(Goodbye(energy = 200)))(result)
    }

    it("successfully parses a a minimal react op code") {
      val result = parseControlCode(
        "React(generation=0,name=foobar,time=100,view=?????WWWWWW_M_WSmsPpBbBbB,energy=100,slaves=0)"
      )

      assertResult(
        Right(React(
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
        Right(React(
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

    it("can parse random inputs") {
      val genViewCell: Gen[Char] = Gen.frequency(
        (1, 'b'),
        (1, 'B'),
        (1, 'p'),
        (1, 'P'),
        (1, 'M'),
        (1, 'm'),
        (1, 's'),
        (1, 'S'),
        (20, '_'),
        (3, 'W'),
        (1, '?')
      )

      val genView: Gen[String] = for {
        viewDistance <- Gen.choose(0, 10)
        sidelength = 11 + viewDistance * 2
        viewCells <- Gen.listOfN(sidelength * sidelength, genViewCell)
      } yield {
        viewCells.mkString
      }

      val genDirection: Gen[String] = Gen.oneOf(
        "0:-1",
        "1:-1",
        "1:0",
        "1:1",
        "0:1",
        "-1:1",
        "-1:0",
        "-1:-1"
      )

      val genPosition: Gen[String] = for {
        x <- Gen.choose(-15, 15)
        y <- Gen.choose(-15, 15)
      } yield {
        s"$x:$y"
      }

      val genPropertyMap: Gen[Map[String, String]] = for {
        size <- Gen.choose(0, 5)
        keys <- Gen.listOfN(size, Gen.alphaStr)
        values <- Gen.listOfN(size, Gen.alphaStr)
      } yield {
        keys.zip(values).toMap
      }

      val genWelcome: Gen[String] = for {
        name <- Gen.identifier
        numRounds <- Gen.choose(1000, 50000)
        currentRound <- Gen.choose(1, numRounds)
        maxSlaves <- Gen.choose(1, 100)
      } yield {
        s"Welcome(name=$name,apocalypse=$numRounds,round=$currentRound,maxslaves=$maxSlaves)"
      }


      def genReact(maxGeneration: Int = 5, maxEnergy: Int = 20000): Gen[String] = for {
        generation <- Gen.choose(0,maxGeneration)
        name <- Gen.identifier
        currentRound <- Gen.choose(1, 10000)
        view <- genView
        currentEnergy <- Gen.choose(0, maxEnergy)
        masterPosition <- Gen.option(genPosition)
        failedMoveDirection <- Gen.option(genDirection)
        numLivingSlaves <- Gen.choose(0, 10)
        extraProperties <- Gen.option(genPropertyMap)
      } yield {
        val baseProperties: List[(String, String)] = List(
          "generation" -> generation.toString,
          "name" -> name,
          "time" -> currentRound.toString,
          "view" -> view,
          "energy" -> currentEnergy.toString,
          "slaves" -> numLivingSlaves.toString
        )

        val optionalProperties: List[(String, String)] = List(
          masterPosition map ("master" -> _),
          failedMoveDirection map ("collision" -> _)
        ).flatten

        val userProperties: List[(String, String)] = extraProperties.getOrElse(Map.empty).toList

        val allProperties = baseProperties ++ optionalProperties ++ userProperties

        s"React(${allProperties.map{case (key, value) => s"$key=$value"}.mkString(",")})"
      }

      val genGoodbye: Gen[String] = for {
        finalEnergy <- Gen.choose(1,1000)
      } yield {
        s"Goodbye(energy=$finalEnergy)"
      }

      val controlOpCodeGenerator: Gen[String] = Gen.frequency(
          (10, genWelcome),
          (10, genGoodbye),
          (80, genReact())
        )

      implicit val shrinker: Shrink[String] = Shrink.shrinkAny

      forAll (controlOpCodeGenerator -> "BotInput") { input: String =>
        assert(parseControlCode(input).isInstanceOf[Right[_, _]])
      }
    }

    it("throws the right error for bad master position") {
      val result = parseControlCode(
        "React(generation=0,name=foobar,time=100,view=?????WWWWWW_M_WSmsPpBbBbB,energy=100,slaves=0,master=1:-2a)"
      )

      assert(result.isLeft)
      assertResult("Unable to parse the master position")(result.left.get.getMessage)
    }

    it("throws the right error for bad collision") {
      val result = parseControlCode(
        "React(generation=0,name=foobar,time=100,view=?????WWWWWW_M_WSmsPpBbBbB,energy=100,slaves=0,collision=1:5b)"
      )

      assert(result.isLeft)
      assertResult("Unable to parse the failed movement direction")(result.left.get.getMessage)
    }

    it("can successfully parse odd directions") {
      val result = parseControlCode(
        "React(generation=0,name=foobar,time=100,view=?????WWWWWW_M_WSmsPpBbBbB,energy=100,slaves=0,collision=1:-59)"
      )

      assert(result.isRight)
      result.right.get match {
        case React(_,_,_,_,_,_,dir,_,_) => assertResult(Some(Direction.DownRight))(dir)
        case _ => fail("Failed to parse to the proper OpCode")
      }
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
