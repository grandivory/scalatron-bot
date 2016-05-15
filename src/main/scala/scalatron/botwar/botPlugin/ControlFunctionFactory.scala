package scalatron.botwar.botPlugin

import com.grandivory.scalatron.bot.Bot
import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.{Direction, RelativePosition, View}

import scala.util.{Failure, Success, Try}

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
  private def parseControlCode(input: String): Either[ControlCodeParseException, ControlOpCode] = {
    val (opCode: String, argList: String) = input.span(_ != '(')

    val arguments: Map[String, String] = argList.stripPrefix("(").stripSuffix(")").split(',').flatMap { argument =>
      argument.split("=") match {
        case arg: Array[String] if 2 == arg.length => Some(arg(0) -> arg(1))
        case _ => None
      }
    }.toMap

    opCode match {
      case "Welcome" => Try {
        Welcome(
          name = arguments("name"),
          numSimulationRounds = arguments("apocalypse").toInt,
          currentRound = arguments("round").toInt,
          maxSlaves = arguments.get("maxslaves").map(_.toInt)
        )
      } match {
        case Success(welcome) => Right(welcome)
        case Failure(exc: Throwable) => Left(new ControlCodeParseException(message = exc.getMessage, input = input, cause = exc))
      }
      case "Goodbye" => Try {
        Goodbye(energy = arguments("energy").toInt)
      } match {
        case Success(goodbye) => Right(goodbye)
        case Failure(exc: Throwable) => Left(new ControlCodeParseException(message = exc.getMessage, input = input, cause = exc))
      }
      case "React" =>
        val extraArguments: Map[String, String] = arguments.filterKeys { key =>
          !Set("generation", "name", "time", "view", "energy", "master", "collision", "slaves").contains(key)
        }

        def opTry2EitherOp[A](in: Option[Try[A]], failMessage: String): Either[ControlCodeParseException, Option[A]] = in match {
          case Some(Success(a)) => Right(Some(a))
          case Some(Failure(e)) => Left(new ControlCodeParseException(failMessage, input, e))
          case None => Right(None)
        }

        // TODO: Map these Option[Try[_]] to Either[ControlCodeParseException, Option[_]]
        // Then, do a for comprehension on their right projections to get the output
        val masterPosition: Option[Try[RelativePosition]] = arguments.get("master").map(RelativePosition.parse)
        val failedMovedDirection: Option[Try[Direction]] = arguments.get("collision").map(Direction.parse)

        for {
          master <- opTry2EitherOp(masterPosition, "Unable to parse the master position").right
          collision <- opTry2EitherOp(failedMovedDirection, "Unable to parse the failed movement direction").right
          controlCode <- (Try {
            React(
              generation = arguments("generation").toInt,
              name = arguments("name"),
              currentRound = arguments("time").toInt,
              view = View(arguments("view")),
              currentEnergy = arguments("energy").toInt,
              masterPosition = master,
              failedMoveDirection = collision,
              numLivingSlaves = arguments("slaves").toInt,
              extraProperties = if (extraArguments.isEmpty) None else Some(extraArguments)
            )
          } match {
            case Success(react) => Right(react)
            case Failure(exc: Throwable) => Left(new ControlCodeParseException(message = exc.getMessage, input = input, cause = exc))
          }).right
        } yield {
          controlCode
        }
      case _ => Left(new ControlCodeParseException("Did not recognize Control Code", input))
    }
  }

  private def serializeBotAction(botCommand: Option[BotCommand]): String = botCommand.map(_.toString).getOrElse("")
}
