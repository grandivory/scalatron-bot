package com.grandivory.scalatron.bot

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.Direction

object Bot {
  /**
    * This is the main function that is called each round that your bot can take an action. Your bot is given an
    * opcode that represents what it should react to, and it must issue a command to perform. The main bot can
    * only react every OTHER round, whereas slave bots can react every round
    */
  def performAction(controlCode: Option[ControlOpCode]): Option[BotCommand] = controlCode match {
    case Some(Welcome(botName, numSimulationRounds, currentRound, maxSlaves)) => None
    case Some(Goodbye(finalEnergy)) => None
    case Some(React(
      generation,
      botName,
      currentRound,
      view,
      currentEnergy,
      masterDirection,
      failedMoveDirection,
      numLivingSlaves,
      extraProps)) =>
      if (currentEnergy >= 100) Some(Spawn(Direction.Up, None, 100, None) + Move(Direction.UpRight))
      else if (generation > 0) Some(Explode(10))
      else Some(Move(Direction.Down))
    case None =>
      Some(Say("?!?!?!?!?") + Move(Direction.Up))
  }
}
