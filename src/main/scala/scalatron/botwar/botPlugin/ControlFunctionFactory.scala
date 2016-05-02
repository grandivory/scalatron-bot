package scalatron.botwar.botPlugin

import com.grandivory.scalatron.bot.Bot

class ControlFunctionFactory {

  def create: String => String = {
    // Parse the input string into a ControlOpCode
    // Pass the ControlOpCode to the real bot
    // Serialize the resulting BotCommand back to a string
    _ => "foobar"
  }
}
