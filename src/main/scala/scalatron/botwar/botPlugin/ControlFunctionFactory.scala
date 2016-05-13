package scalatron.botwar.botPlugin

import com.grandivory.scalatron.bot.Bot
import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.{Direction, RelativePosition, View}

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
  private def parseControlCode(input: String): Option[ControlOpCode] = {
    val (opCode: String, argList: String) = input.span(_ != '(')

    val arguments: Map[String, String] = argList.stripPrefix("(").stripSuffix(")").split(',').flatMap { argument =>
      argument.split("=") match {
        case arg: Array[String] if 2 == arg.length => Some(arg(0) -> arg(1))
        case _ => None
      }
    }.toMap

    opCode match {
      case "Welcome" =>
        for {
          name <- arguments.get("name")
          numSimulationRounds <- arguments.get("apocalypse")
          currentRound <- arguments.get("round")
          maxSlaves <- arguments.get("maxslaves")
        } yield {
          Welcome(
            name = name,
            numSimulationRounds = numSimulationRounds.toInt,
            currentRound = currentRound.toInt,
            maxSlaves = maxSlaves.toInt
          )
        }
      case "Goodbye" =>
        for {
          energy <- arguments.get("energy")
        } yield {
          Goodbye(energy = energy.toInt)
        }
      case "React" =>
        val extraArguments: Map[String, String] = arguments.filterKeys { key =>
          !Set("generation", "name", "time", "view", "energy", "master", "collision", "slaves").contains(key)
        }
        for {
          generation <- arguments.get("generation")
          name <- arguments.get("name")
          currentRound <- arguments.get("time")
          view <- arguments.get("view")
          currentEnergy <- arguments.get("energy")
          numLivingSlaves <- arguments.get("slaves")
          extraProperties = if (extraArguments.isEmpty) None else Some(extraArguments)
        } yield {
          React(
            generation = generation.toInt,
            name = name,
            currentRound = currentRound.toInt,
            view = View(view),
            currentEnergy = currentEnergy.toInt,
            masterPosition = arguments.get("master").map(RelativePosition.parse),
            failedMoveDirection = arguments.get("collision").map(Direction.parse),
            numLivingSlaves = numLivingSlaves.toInt,
            extraProperties = extraProperties
          )
        }
      case _ => None
    }
  }

  private def serializeBotAction(botCommand: Option[BotCommand]): String = botCommand.map(_.toString).getOrElse("")
}
