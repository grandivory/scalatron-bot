package scalatron.botwar.botPlugin

import com.grandivory.scalatron.bot.Bot
import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.{Direction, View}

class ControlFunctionFactory {

  def create: String => String =
    // Parse the input string into a ControlOpCode
    // Pass the ControlOpCode to the real bot
    // Serialize the resulting BotCommand back to a string
    parseControlCode _ andThen Bot.performAction andThen serializeBotAction

  /**
    * Given a string input, parse out a full ControlOpCode representation of that input
    *
    * @param input The string input to parse over
    */
  private def parseControlCode(input: String): ControlOpCode = {
    val (opCode: String, argList: String) = input.span(_ != '(')

    val arguments: Map[String, String] = argList.stripPrefix("(").stripSuffix(")").split(',').flatMap { argument =>
      argument.split("=") match {
        case arg: Array[String] if 2 == arg.length => Some(arg(0) -> arg(1))
        case _ => None
      }
    }.toMap

    opCode match {
      case "Welcome" => Welcome(
        name = arguments("name"),
        numSimulationRounds = arguments("apocalypse").toInt,
        currentRound = arguments("round").toInt,
        maxSlaves = arguments("maxslaves").toInt
      )
      case "Goodbye" => Goodbye(
        energy = arguments("energy").toInt
      )
      case "React" =>
        val extraArguments: Map[String, String] = arguments.filterKeys { key =>
          val knownArguments = Set("generation", "name", "time", "view", "energy", "master", "collision", "slaves")

          !knownArguments.contains(key)
        }
        React(
          generation = arguments("generation").toInt,
          name = arguments("name"),
          currentRound = arguments("time").toInt,
          view = View(arguments("view")),
          currentEnergy = arguments("energy").toInt,
          masterDirection = arguments.get("master").map(Direction.parse),
          failedMoveDirection = arguments.get("collision").map(Direction.parse),
          numLivingSlaves = arguments("slaves").toInt,
          extraProperties = if (extraArguments.isEmpty) None else Some(extraArguments)
        )
    }
  }

  private def serializeBotAction(botCommand: BotCommand): String = botCommand.toString
}
