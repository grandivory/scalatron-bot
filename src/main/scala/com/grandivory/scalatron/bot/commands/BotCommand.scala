package com.grandivory.scalatron.bot.commands

import com.grandivory.scalatron.bot.util.{Color, Direction, RelativePosition}

/**
  * This represents a command to send to your bot.
  */
trait BotCommand {
  // TODO: Handle command collision better
  def +(command: BotCommand): BotCommand = this match {
    case MultiCommand(commands) => MultiCommand(commands + command)
    case _ => MultiCommand(Set(command + this))
  }

  def ++(commands: Set[BotCommand]): BotCommand = this match {
    case MultiCommand(oldCommands: Set[BotCommand]) => MultiCommand(commands ++ oldCommands)
    case _ => MultiCommand(commands + this)
  }
}

/**
  * Tell the bot to do nothing
  */
case object Noop extends BotCommand {
  override def toString: String = ""
}

/**
  * Bots can move exactly one space in any direction
  *
  * @param direction The direction for the bot to move in
  */
case class Move(direction: Direction) extends BotCommand {
  override def toString: String = s"Move($direction)"
}

/**
  * Spawn a slave bot that reacts independently of the main bot
  *
  * @param direction What direction to spawn the slave in. The slave will spawn 1 square away from the main bot
  * @param name The name of the slave bot. If this is empty, the slave will be given a name by Scalatron
  * @param energy The amount of energy to siphon to the slave bot. This can't be less than 100
  * @param slaveProperties A map of extra properties to assign to the slave bot that will be passed in each React call
  */
case class Spawn(direction: Direction,
                 name: Option[String],
                 energy: Int = 100,
                 slaveProperties: Option[Map[String, String]])
  extends BotCommand {
  override def toString: String = s"Spawn($direction,"
}

/**
  * Set extra properties on this bot that will be passed back in each React call
  */
case class SetProperties(properties: Map[String, String]) extends BotCommand

/**
  * Destroy this bot, dealing damage spread out over the given radius and
  * transferring the damage dealt as energy back to the main bot
  *
  * @param blastRadius number of squares over which to spread the damage
  */
case class Explode(blastRadius: Int) extends BotCommand

/**
  * Display a text bubble on the board, up to 10 characters, that stays in place as the bot moves
  *
  * The text can't contain '|', ',', '=', or '('
  */
case class Say(text: String) extends BotCommand

/**
  * Display a text bubble, up to 20 characters, that moves with the bot
  *
  * The text can't contain '|', ',', '=', or '('
  */
case class Status(text: String) extends BotCommand

/**
  * Mark a cell with a particular color
  *
  * @param position The cell to mark
  * @param color The color to shade the cell
  */
case class MarkCell(position: RelativePosition, color: Color)

/**
  * Draw a line between two cells
  */
case class DrawLine(from: RelativePosition, to: RelativePosition, color: Color)

/**
  * A shortcut for SetProperties(Map("debug" -> text))
  */
case class Log(text: String)

/**
  * Perform multiple commands in a single round. Note that only one command
  * of any given type will ever be executed in a round.
  *
  * @param commands A set of all commands to execute
  */
case class MultiCommand private(commands: Set[BotCommand]) extends BotCommand
