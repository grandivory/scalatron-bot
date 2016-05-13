package com.grandivory.scalatron.bot

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.ViewObject.Empty
import com.grandivory.scalatron.bot.util._

object Bot {

  val ROLE = "role"
  val ROLE_GATHERER = "gatherer"
  val ROLE_MISSILE = "missile"
  val ROLE_CHAFF = "chaff"

  val MAX_WORKER_SLAVES = 6
  val MAX_SLAVES = 10
  val MAX_SLAVE_ENERGY = 2000
  val MAX_SLAVE_ROUND = 4900

  type BotStrategy = () => Option[BotCommand]

  val masterMovementTargets: Set[ViewObject] = Set(
    Empty,
    Zugar,
    Slave,
    Fluppet
  )

  val slaveBotMovementTargets: Set[ViewObject] = masterMovementTargets - Slave + MasterBot

  def randomWalk(view: View, moveTargets: Set[ViewObject]): BotStrategy = () => {
    val randomDir = Direction.randomDirection
    for {
      moveDir <- closestDirectionTo(randomDir, view, moveTargets)
    } yield {
      Move(moveDir) + Status("???")
    }
  }

  def spawnWorker(energy: Int, numSlaves: Int, round: Int): BotStrategy = () =>
    if (energy > 100 && numSlaves < MAX_WORKER_SLAVES && round < MAX_SLAVE_ROUND)
      Some(Spawn(Direction.randomDirection, None, 100, Some(Map(ROLE -> ROLE_GATHERER))))
    else
      None

  def spawnMissile(energy: Int, numSlaves: Int, round: Int, view: View): BotStrategy = () =>
    if (energy > 100 && numSlaves < MAX_SLAVES && round < MAX_SLAVE_ROUND) for {
      enemy <- view.nearest(obj => EnemyBot == obj || EnemySlave == obj)
      enemyDirection <- enemy.direction
      spawnDirection <- closestDirectionTo(enemyDirection, view, slaveBotMovementTargets)
    } yield {
      Spawn(spawnDirection, None, 100, Some(Map(ROLE -> ROLE_MISSILE)))
    }
    else None

  def spawnChaff(energy: Int, numSlaves: Int, round: Int, view: View): BotStrategy = () =>
    if (energy > 100 && numSlaves < MAX_SLAVES && round < MAX_SLAVE_ROUND) for {
      enemyMissile <- view.nearest(EnemySlave == _)
      missileDirection <- enemyMissile.direction
      spawnDirection <- closestDirectionTo(missileDirection, view, slaveBotMovementTargets)
    } yield {
      Spawn(spawnDirection, None, 100, Some(Map(ROLE -> ROLE_CHAFF)))
    }
    else None

  def goHome(masterPosition: RelativePosition, view: View): BotStrategy = () => for {
    masterDirection <- masterPosition.direction
    moveDirection <- closestDirectionTo(masterDirection, view, slaveBotMovementTargets)
  } yield {
    Move(moveDirection) + Status("HOME")
  }

  def finishGathering(energy: Int, round: Int, masterPosition: RelativePosition, view: View): BotStrategy = () =>
    if (energy > MAX_SLAVE_ENERGY || round > MAX_SLAVE_ROUND)
      goHome(masterPosition, view)()
    else None

  def seekAndDestroy(view: View, targets: Set[ViewObject]): BotStrategy = () => {
    for {
      nearestEnemy <- view.nearest(targets.contains)
      enemyDirection <- nearestEnemy.direction
      moveTowardEnemy <- closestDirectionTo(enemyDirection, view, slaveBotMovementTargets)
    } yield {
      if (nearestEnemy.euclidianDistance < 3) Explode(4)
      else Move(moveTowardEnemy) + Status("PEW PEW")
    }
  }

  val becomeGatherer: BotStrategy = () => Some(SetProperties(Map(ROLE -> ROLE_GATHERER)))


  /**
    * This is the main function that is called each round that your bot can take an action. Your bot is given an
    * opcode that represents what it should react to, and it must issue a command to perform. The main bot can
    * only react every OTHER round, whereas slave bots can react every round
    */
  def performAction(controlCode: Option[ControlOpCode]): Option[BotCommand] = controlCode match {
    case Some(React(
      generation,
      botName,
      currentRound,
      view,
      currentEnergy,
      masterPosition,
      failedMoveDirection,
      numLivingSlaves,
      extraProps)) =>

      if (0 == generation) actAsMaster(currentRound, view, currentEnergy, numLivingSlaves)
      else for {
        slaveProperties: Map[String, String] <- extraProps
        role: String <- slaveProperties.get(ROLE)
        home: RelativePosition <- masterPosition
        command: BotCommand <- actAsSlave(currentRound, view, currentEnergy, home, role)
      } yield {
        command + Log(role)
      }

    case None =>
      Some(Say("?!?!?!?!?") + Move(Direction.Up))
    case _ => None
  }

  private def actAsMaster(round: Int, view: View, energy: Int, numSlaves: Int): Option[BotCommand] = {
    andAlso(
      spawnChaff(energy,numSlaves, round, view)() orElse
        spawnMissile(energy, numSlaves, round, view)() orElse
        spawnWorker(energy, numSlaves, round)(),
      gather(view, masterMovementTargets) orElse randomWalk(view, masterMovementTargets)()
    )
  }

  private def actAsSlave(round: Int, view: View, energy: Int, masterPosition: RelativePosition, role: String):
  Option[BotCommand] = {
    role match {
      case ROLE_GATHERER => finishGathering(energy, round, masterPosition, view)() orElse
        gather(view, slaveBotMovementTargets) orElse
        goHome(masterPosition, view)()
      case ROLE_MISSILE => seekAndDestroy(view, Set(EnemyBot))() orElse becomeGatherer()
      case ROLE_CHAFF => seekAndDestroy(view, Set(EnemyBot, EnemySlave))() orElse becomeGatherer()
      case _ => None
    }
  }

  private def gather(view: View, moveTargets: Set[ViewObject]): Option[BotCommand] = {
    val avoidDamage: BotStrategy = () => for {
      threat <- view.nearest(obj => Snorg == obj || EnemyBot == obj || EnemySlave == obj) if threat.distance <= 2
      threatDirection <- threat.direction
      moveDir <- closestDirectionTo(threatDirection.reverse, view, moveTargets)
    } yield {
      Move(moveDir) + Status("AAAAAAAAAH!")
    }

    val eatFood: BotStrategy = () => for {
      nearestFood <- view.nearest(obj => Zugar == obj || Fluppet == obj)
      foodDirection <- nearestFood.direction
      moveDir <- closestDirectionTo(foodDirection, view, moveTargets)
    } yield {
      Move(moveDir) + Status("OM NOM NOM")
    }

    val playItSafe: BotStrategy = () => for {
      nearestEnemy <- view.nearest(obj => Snorg == obj || EnemyBot == obj || EnemySlave == obj)
      threatDirection <- nearestEnemy.direction
      safeDirection <- closestDirectionTo(threatDirection.reverse, view, moveTargets)
    } yield {
      Move(safeDirection) + Status("Run Away!")
    }

    avoidDamage() orElse eatFood() orElse playItSafe()
  }

  private def safeToMove(direction: Direction, view: View, safeSet: Set[ViewObject]): Boolean = {
    view.objectAt(direction.toRelativePosition) match {
      case Some(Empty) => true
      case Some(go: GameObject) => safeSet.contains(go)
      case None => false
    }
  }

  private def closestDirectionTo(direction: Direction, view: View, movableObjects: Set[ViewObject]): Option[Direction] = {
    val allDirections = List(
      direction,
      direction.right45,
      direction.left45,
      direction.right90,
      direction.left90,
      direction.reverse.left45,
      direction.reverse.right45,
      direction.reverse
    )

    allDirections.find(d =>
      safeToMove(d, view, movableObjects)
    )
  }

  private def nearestSafeCell(cell: RelativePosition, view: View): Option[RelativePosition] = {
    view.objectsInView.keys.toList.sortBy(_ - cell).find {
      view.objectAt(_) match {
        case Some(Empty) => true
        case Some(go: GameObject) => go.isGood
        case _ => false
      }
    }
  }

  private def shortestPathTo(cell: RelativePosition, view: View): Option[List[Direction]] = {
    def okToMove(objectInSpot: Option[ViewObject]): Boolean = objectInSpot match {
      case Some(Empty) => true
      case Some(gameObject: GameObject) => gameObject.isGood
      case None => false
    }

    def nextNodes(pathSoFar: Vector[Direction], position: RelativePosition, visited: Set[RelativePosition]):
      Set[(Vector[Direction], RelativePosition)] = {
      Direction.allPossibleDirections map { direction: Direction =>
        (pathSoFar :+ direction) -> (position + direction)
      } filter {
        case (_: Vector[Direction], newPosition: RelativePosition) =>
          !visited.contains(newPosition) &&
            okToMove(view.objectAt(newPosition))
      }
    }

    if (Origin == cell) Some(Nil)
    else if (!okToMove(view.objectAt(cell))) None
    else {
      val allPaths: Stream[(Set[(Vector[Direction], RelativePosition)], Set[RelativePosition])] =
        Stream.iterate((Set((Vector.empty[Direction], Origin: RelativePosition)), Set[RelativePosition](Origin))) {
          case (moveSet: Set[(Vector[Direction], RelativePosition)], visitedPositions) =>
            val nextMoveSets = moveSet flatMap {
              case (directions, currentPosition) =>
                nextNodes(directions, currentPosition, visitedPositions)
            }

            val newVisitedPositions = visitedPositions ++ nextMoveSets.map(_._2)
            (nextMoveSets, newVisitedPositions)
        } takeWhile {
          case (moveSet: Set[(Vector[Direction], RelativePosition)], visitedCells: Set[RelativePosition]) =>
            moveSet.nonEmpty
        }

      allPaths.take(10).flatMap(_._1.toStream).collectFirst {
        case (directions: Vector[Direction], endPosition: RelativePosition) if endPosition == cell => directions.toList
      }
    }
  }

  private def andAlso(left: Option[BotCommand], right: Option[BotCommand]): Option[BotCommand] = {
    (left, right) match {
      case (Some(l), Some(r)) => Some(l + r)
      case (Some(l), None) => left
      case (None, Some(r)) => right
      case (None, None) => None
    }
  }
}
