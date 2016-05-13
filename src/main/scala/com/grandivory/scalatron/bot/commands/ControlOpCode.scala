package com.grandivory.scalatron.bot.commands

import com.grandivory.scalatron.bot.util.{Direction, RelativePosition, View}

/**
  * This is a control code that will be sent to the bot when it needs to do something
  */
sealed trait ControlOpCode extends Product with Serializable

/**
  * Code sent when a bot is first generated. For main bots, this will be sent in the first round. For slaves, this will
  * be sent when the slave is created. This gives the bot time to set itself up if need be.
  *
  * @param name The name of this bot
  * @param numSimulationRounds The number of total rounds in the simulation
  * @param currentRound The number of the current round
  * @param maxSlaves The total number of slaves that bots are allowed to have
  */
case class Welcome(name: String, numSimulationRounds: Int, currentRound: Int, maxSlaves: Int) extends ControlOpCode

/**
  * Code sent when the simulation ends or the bot will be destroyed. If anything needs to be cleaned up from memory,
  * this is the time to do it.
  *
  * @param energy The amount of energy this bot has left
  */
case class Goodbye(energy: Int) extends ControlOpCode

/**
  * Code sent for "standard" actions. This is what will be sent every round (or every other round, for the main bot) to
  * allow a bot to act in the simulation.
  *
  * @param generation What generation this bot is. 0 for the main bot, 1 for first-generation slaves, etc.
  * @param name The name of this bot
  * @param currentRound The current round of the simulation
  * @param view Everything that the bot can currently see
  * @param currentEnergy The amount of energy the bot currently has
  * @param masterPosition For slaves, the relative position of their master
  * @param failedMoveDirection If a previous move failed, this is the direction of the move attempt
  * @param numLivingSlaves The number of slave bots that are currently alive
  * @param extraProperties Any extra properties that were set on the bot at creation time,
  *                        or with the SetProperties command
  */
case class React(generation: Int,
                 name: String,
                 currentRound: Int,
                 view: View,
                 currentEnergy: Int,
                 masterPosition: Option[RelativePosition] = None,
                 failedMoveDirection: Option[Direction] = None,
                 numLivingSlaves: Int,
                 extraProperties: Option[Map[String, String]] = None)
  extends ControlOpCode
