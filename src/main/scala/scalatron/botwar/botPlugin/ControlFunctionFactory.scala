package scalatron.botwar.botPlugin

import com.grandivory.scalatron.bot.Bot
import com.grandivory.scalatron.bot.commands._

object ControlFunctionFactory {

  def create: String => String = { input: String =>
    // Parse the input string into a ControlOpCode
    // Pass the ControlOpCode to the real bot
    // Serialize the resulting BotCommand back to a string
    serializeBotAction(Bot.performAction(Welcome("foobar", 12, 3, 4)))
  }

  /**
    * Given a string input, parse out a full ControlOpCode representation of that input
    *
    * @param input The string input to parse over
    */
  private def parseControlCode(input: String): ControlOpCode = {
    val (opCode: String, argList: String) = input.span(_ != '(')

    val arguments: List[String] = argList.dropRight(1).split(',').toList

//    opCode match {
//      case "Welcome" => Welcome()
//      case "React" => React()
//      case "Goodbye" => Goodbye()
//    }
    ???
  }

  private def serializeBotAction(botCommand: BotCommand): String = botCommand.toString
}
