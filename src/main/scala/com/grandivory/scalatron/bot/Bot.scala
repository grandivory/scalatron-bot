package com.grandivory.scalatron.bot

import com.grandivory.scalatron.bot.commands._

object Bot {
  /**
    * This is the main function that is called each round that your bot can take an action. Your bot is given an
    * opcode that represents what it should react to, and it must issue a command to perform. The main bot can
    * only react every OTHER round, whereas slave bots can react every round
    */
  def performAction(controlCode: ControlOpCode): BotCommand = {
    Status("FOO!")
  }
}
