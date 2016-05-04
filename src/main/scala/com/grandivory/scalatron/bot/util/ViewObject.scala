package com.grandivory.scalatron.bot.util

sealed trait ViewObject
object ViewObject {
  case object Empty extends ViewObject
}

sealed trait GameObject extends ViewObject {
  def isGood: Boolean
  def canMove: Boolean
}

case object Wall extends GameObject {
  override val isGood = false
  override val canMove = false
}

sealed trait Beast extends GameObject {
  override val canMove = true
}

case object Snorg extends Beast {
  override val isGood: Boolean = false
}
case object Fluppet extends Beast {
  override val isGood: Boolean = true
}

sealed trait Plant extends GameObject {
  override val canMove = false
}

case object Zugar extends Plant {
  override val isGood: Boolean = true
}
case object Toxifera extends Plant {
  override val isGood: Boolean = false
}

sealed trait Enemy extends GameObject {
  override val canMove = true
  override val isGood = false
}

case object EnemyBot extends Enemy
case object EnemySlave extends Enemy

case object MasterBot extends GameObject {
  override val isGood = true
  override val canMove = true
}
case object Slave extends GameObject {
  override val isGood = true
  override val canMove = true
}
