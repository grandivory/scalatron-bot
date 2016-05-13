package com.grandivory.scalatron.bot

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.ViewObject.Empty
import com.grandivory.scalatron.bot.util._

object Bot {

  type BotStrategy = () => Option[BotCommand]

  val ROLE = "mood"
  val ROLE_GATHERER = "gatherer"
  val ROLE_MISSILE = "missile"
  val ROLE_CHAFF = "chaff"

  val MAX_WORKER_SLAVES = 6
  val MAX_SLAVES = 10
  val MAX_ENERGY = 2000

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
      masterDirection,
      failedMoveDirection,
      numLivingSlaves,
      extraProps)) =>

      if (0 == generation) actAsMaster(currentRound, view, currentEnergy, numLivingSlaves)
      else for {
        slaveProperties: Map[String, String] <- extraProps
        role: String <- slaveProperties.get(ROLE)
        home: Direction <- masterDirection
        command: BotCommand <- actAsSlave(currentRound, view, currentEnergy, home, role)
      } yield {
        command + Log(role)
      }

      /***************
        * Strategies *
        **************/
//      val pathToFood: BotStrategy = () => for {
//        nearestFood <- nearestFoodOp
//        shortestPath <- shortestPathTo(nearestFood, view)
//        firstMove <- shortestPath.headOption
//      } yield {
//        Move(firstMove) + Status("FOOD!")
//      }
//
//      val moveTowardFood: BotStrategy = () => for {
//        nearestFood <- nearestFoodOp
//        foodDirection <- nearestFood.direction
//        moveDirection <- closestDirectionTo(foodDirection, view)
//      } yield {
//        Move(moveDirection) + Status("food?")
//      }
//
//      val runFromEnemy: BotStrategy = () => for {
//        nearestEnemy <- view.nearest(obj => Snorg == obj || EnemySlave == obj || EnemyBot == obj)
//        enemyDirection <- nearestEnemy.direction
//        runDirection <- closestDirectionTo(enemyDirection.reverse, view)
//      } yield {
//        Move(runDirection) + Status("Run Away!")
//      }
//
//      val stumped: BotStrategy = () => {
//        val randomDir = Direction.randomDirection
//        for {
//          moveDirection <- closestDirectionTo(randomDir, view)
//        } yield {
//          Move(moveDirection) + Status("???")
//        }
//      }
//
//      val missile: BotStrategy = () => {
//        None
//      }

//      pathToFood() orElse moveTowardFood() orElse runFromEnemy() orElse stumped()
    case None =>
      Some(Say("?!?!?!?!?") + Move(Direction.Up))
    case Some(command@Welcome(_, _, _, _)) => println(command); None
    case Some(command@Goodbye(_)) => println(command); None
    case _ => None
  }

  private def actAsMaster(round: Int, view: View, energy: Int, numSlaves: Int): Option[BotCommand] = {
    val spawnWorker: BotStrategy = () =>
      if (energy > 100 && numSlaves < MAX_WORKER_SLAVES)
        Some(Spawn(Direction.randomDirection, None, 100, Some(Map(ROLE -> ROLE_GATHERER))))
      else
        None

    val spawnMissile: BotStrategy = () => for {
      enemy <- view.nearest(obj => EnemyBot == obj || EnemySlave == obj) if energy > 100 && numSlaves < MAX_SLAVES
      enemyDirection <- enemy.direction
      spawnDirection <- closestDirectionTo(enemyDirection, view)
    } yield {
      Spawn(spawnDirection, None, 100, Some(Map(ROLE -> ROLE_MISSILE)))
    }

    val spawnChaff: BotStrategy = () => for {
      enemyMissile <- view.nearest(EnemySlave == _) if enemyMissile.distance <= 3 && energy > 100 && numSlaves < MAX_SLAVES
      missileDirection <- enemyMissile.direction
      spawnDirection <- closestDirectionTo(missileDirection, view)
    } yield {
      Spawn(spawnDirection, None, 100, Some(Map(ROLE -> ROLE_CHAFF)))
    }

    andAlso(spawnChaff() orElse spawnMissile() orElse spawnWorker(), gather(view))
  }

  private def actAsSlave(round: Int, view: View, energy: Int, masterDirection: Direction, role: String):
  Option[BotCommand] = {
    val returnHome: BotStrategy = () => for {
      moveDirection <- closestDirectionTo(masterDirection, view)
    } yield {
      Move(moveDirection) + Status("HOME")
    }

    println("Slave bot!")

    role match {
      case ROLE_GATHERER => Some(Status("FARMER"))
      case ROLE_MISSILE => Some(Status("PEW PEW!!"))
      case ROLE_CHAFF => Some(Status("AEGIS"))
      case _ => None
    }
  }

  private def gather(view: View): Option[BotCommand] = {
    val avoidDamage: BotStrategy = () => for {
      threat <- view.nearest(obj => Snorg == obj || EnemyBot == obj || EnemySlave == obj) if threat.distance <= 2
      threatDirection <- threat.direction
      moveDir <- closestDirectionTo(threatDirection.reverse, view)
    } yield {
      Move(moveDir) + Status("AAAAAAAAAH!")
    }

    val eatFood: BotStrategy = () => for {
      nearestFood <- view.nearest(obj => Zugar == obj || Fluppet == obj)
      foodDirection <- nearestFood.direction
      moveDir <- closestDirectionTo(foodDirection, view)
    } yield {
      Move(moveDir) + Status("OM NOM NOM")
    }

    val playItSafe: BotStrategy = () => for {
      nearestEnemy <- view.nearest(obj => Snorg == obj || EnemyBot == obj || EnemySlave == obj)
      threatDirection <- nearestEnemy.direction
      safeDirection <- closestDirectionTo(threatDirection.reverse, view)
    } yield {
      Move(safeDirection) + Status("Run Away!")
    }

    val randomWalk: BotStrategy = () => {
      val randomDir = Direction.randomDirection
      for {
        moveDir <- closestDirectionTo(randomDir, view)
      } yield {
        Move(moveDir) + Status("???")
      }
    }

    avoidDamage() orElse eatFood() orElse playItSafe() orElse randomWalk()
  }

  private def safeToMove(direction: Direction, view: View): Boolean = {
    view.objectAt(direction.toRelativePosition) match {
      case Some(Empty) => true
      case Some(go: GameObject) => go.isGood
      case None => false
    }
  }

  private def closestDirectionTo(direction: Direction, view: View): Option[Direction] = {
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
      safeToMove(d, view)
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
