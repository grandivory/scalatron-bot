package com.grandivory.scalatron.bot.commands

import com.grandivory.scalatron.bot.util.{Color, Direction, RelativePosition}

/**
  * This represents a command to send to your bot.
  */
trait BotCommand extends Product with Serializable {
  // TODO: Handle command collision better
  def +(command: BotCommand): BotCommand = this match {
    case MultiCommand(commands) => MultiCommand(commands :+ command)
    case _ => MultiCommand(List(this, command))
  }

  def ++(commands: List[BotCommand]): BotCommand = this match {
    case MultiCommand(oldCommands: List[BotCommand]) => MultiCommand(oldCommands ::: commands)
    case _ => MultiCommand(this :: commands)
  }

  override def toString: String = {
    val fieldMap: Map[String, Any] = this.getClass.getDeclaredFields.map(_.getName)
      .zip(this.productIterator.to).toMap[String, Any]

    val printableFields: List[(String, Any)] = fieldMap.foldRight(List.empty[(String, Any)]) {
      (newField: (String, Any), fieldMap: List[(String, Any)]) => newField match {
        case (fieldName, Some(multiField: Map[_, _])) => multiField.map {
            case (key, value) => key.toString -> value
          }.toList ::: fieldMap
        case (fieldName, Some(value)) => (fieldName -> value) :: fieldMap
        case (fieldName, None) => fieldMap
        case (fieldName, value) => (fieldName -> value) :: fieldMap
      }
    }

    val fieldValues: String = printableFields map {
      case (fieldName, fieldValue) => s"$fieldName=$fieldValue"
    } mkString ","

    this.getClass.getSimpleName + "(" + fieldValues + ")"
  }
}

/**
  * Bots can move exactly one space in any direction
  *
  * @param direction The direction for the bot to move in
  */
case class Move(direction: Direction) extends BotCommand

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
  extends BotCommand

/**
  * Set extra properties on this bot that will be passed back in each React call
  */
case class SetProperties(properties: Map[String, String]) extends BotCommand {
  override def toString: String =
    "Set(" + properties.map {
      case (key, value) => s"$key=$value"
    }.mkString(",") + ")"
}

/**
  * Destroy this bot, dealing damage spread out over the given radius and
  * transferring the damage dealt as energy back to the main bot
  *
  * @param blastRadius number of squares over which to spread the damage
  */
case class Explode(blastRadius: Int) extends BotCommand {
  override def toString: String = s"Explode(size=$blastRadius)"
}

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
case class MarkCell(position: RelativePosition, color: Color) extends BotCommand

/**
  * Draw a line between two cells
  */
case class DrawLine(from: RelativePosition, to: RelativePosition, color: Color) extends BotCommand

/**
  * A shortcut for SetProperties(Map("debug" -> text))
  */
case class Log(text: String) extends BotCommand

/**
  * Perform multiple commands in a single round. Note that only one command
  * of any given type will ever be executed in a round.
  *
  * @param commands A set of all commands to execute
  */
case class MultiCommand private(commands: List[BotCommand]) extends BotCommand {
  override def toString: String = commands.mkString("|")
}
