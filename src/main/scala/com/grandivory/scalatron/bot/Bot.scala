package com.grandivory.scalatron.bot

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.ViewObject.Empty
import com.grandivory.scalatron.bot.util._

import scala.util.Random

object Bot {

  val ROLE_MISSILE = "missile"
  val ROLE_GATHERER = "gatherer"
  val GATHERER_MAX_ENERGY = 2000

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

//      val role: String = (for {
//        props <- extraProps
//        role: String <- props.get("role")
//      } yield role).getOrElse(ROLE_GATHERER)

      val nearestFood = view.nearest { obj: ViewObject =>
        Zugar == obj || Fluppet == obj
      }

//      (for {
//        foodPosition <- nearestFood
//        foodDirection <- foodPosition.direction
//      } yield {
//        Move(foodDirection) + Status("searchin'")
//      }) orElse {
//        Some(Move(Direction.randomDirection) + Status("..."))
//      }


      for {
        foodPosition <- nearestFood
        pathToFood <- shortestPathTo(foodPosition, view)
        firstMove <- pathToFood.headOption
      } yield {
        Move(firstMove) + Status("Walk this way!")
      }


//      (for {
//        foodPosition <- nearestFood
//        shortestPath <- shortestPathTo(foodPosition, view)
//        firstMove <- shortestPath.headOption
//      } yield {
//        Move(firstMove) + Status("RAARGH")
//      }) orElse {
//        for {
//          closestSafeCell <- nearestSafeCell(Direction.UpRight * view.distance, view)
//          pathToCell <- shortestPathTo(closestSafeCell, view)
//          firstMove <- pathToCell.headOption
//        } yield {
//          Move(firstMove) + Status("HUH?!?")
//        }
//      }
////      role match {
////        // Gatherer:
////        case ROLE_GATHERER if generation > 0 && currentEnergy > GATHERER_MAX_ENERGY =>
////          // If we're a slave and over our max energy, head back toward the main bot
////          val moveCommand: Option[Move] =
////            // If we can see the master bot, find the shortest path to it
////            (for {
////              masterPosition <- view.objectsInView.toList.map(_.swap).toMap.get(MasterBot)
////              shortestPath <- shortestPathTo(masterPosition, view)
////              firstMove <- shortestPath.headOption
////            } yield {
////              Move(firstMove)
////            }) orElse {
////              // If we can't see the master bot, try to move in its direction, at least
////              for {
////                movementDirection <- masterDirection
////                movementTarget <- nearestSafeCell(movementDirection * view.distance, view)
////                shortestPath <- shortestPathTo(movementTarget, view)
////                firstMove <- shortestPath.headOption
////              } yield {
////                Move(firstMove)
////              }
////            }
////
////          moveCommand.map(_ + Status("Comin' home!"))
////
////        case ROLE_GATHERER =>
////          // Otherwise, look for the nearest food item.
////          // If we can, spawn a slave to go grab it (up to 2 levels deep)
////          // Otherwise, grab it ourselves
////          // If there's nothing to grab, pick an open direction and start moving
////          val nearestFood: Option[RelativePosition] = view.nearest { obj: ViewObject =>
////            Fluppet == obj || Zugar == obj
////          }
////          val nearestFoodDirection: Option[Direction] = for {
////            target <- nearestFood
////            shortestPath <- shortestPathTo(target, view)
////            direction <- shortestPath.headOption
////          } yield {
////            direction
////          }
////
////          val spawnCommand =
////            if (generation > 0 || currentEnergy < 100) None
////            else nearestFoodDirection map { spawnDirection =>
////              Spawn(
////                direction = spawnDirection,
////                name = None,
////                energy = 100,
////                slaveProperties = Some(Map("role" -> ROLE_GATHERER))
////              )
////            }
////
////          val moveCommand = spawnCommand match {
////            case Some(Spawn(spawnDirection, _, _, _)) => Some(Move(spawnDirection.reverse))
////            case None => nearestFoodDirection map { moveDirection =>
////              Move(moveDirection)
////            }
////          }
////
////          // Also, if there's any enemies, spawn a missile in their direction
////          val missileCommand = spawnCommand match {
////            case None if 0 == generation && currentEnergy >= 100 =>
////              val nearestEnemy: Option[RelativePosition] = view.nearest { obj: ViewObject =>
////                Set[ViewObject](EnemyBot, EnemySlave, Snorg) contains obj
////              }
////              for {
////                enemyPosition <- nearestEnemy
////                shortestPath <- shortestPathTo(enemyPosition, view)
////                spawnDirection <- shortestPath.headOption
////              } yield {
////                Spawn(
////                  direction = spawnDirection,
////                  name = None,
////                  energy = 100,
////                  slaveProperties = Some(Map("role" -> ROLE_MISSILE))
////                )
////              }
////            case _ => None
////          }
////
////          for {
////            move <- moveCommand
////            spawn <- spawnCommand
////            missile <- missileCommand
////          } yield {
////            move + spawn + missile + Status("Searchin'")
////          }
////
////        case ROLE_MISSILE =>
////          val nearestEnemy: Option[RelativePosition] = view.nearest { obj: ViewObject =>
////            Set[ViewObject](EnemyBot, EnemySlave, Snorg) contains obj
////          }
////
////          (nearestEnemy match {
////            // If there's an enemy within 2 cells, explode!
////            case Some(position) if position.distance <= 2 => Some(Explode(3))
////            // Otherwise, seek the nearest enemy
////            case Some(position) => for {
////                shortestPath <- shortestPathTo(position, view)
////              } yield {
////              Move(shortestPath.head)
////            }
////            // If everyone is out of range, turn into a gatherer
////            case None => Some(SetProperties(Map("role" -> ROLE_GATHERER)))
////          }) map {
////          _ + Status("PEW!")
////        }
////      }
    case None =>
      Some(Say("?!?!?!?!?") + Move(Direction.Up))
    case _ => None
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

    val allPaths: Stream[(Vector[Direction], RelativePosition)] =
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
      } flatMap (_._1.toStream)

    allPaths.collectFirst {
      case (directions: Vector[Direction], endPosition: RelativePosition) if endPosition == cell => directions.toList
    }
  }
}
