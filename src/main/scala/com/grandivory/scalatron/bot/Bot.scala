package com.grandivory.scalatron.bot

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.ViewObject.Empty
import com.grandivory.scalatron.bot.util._

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

      val nearestFoodOp = view.nearest(viewObject => Zugar == viewObject || Fluppet == viewObject)

      def safeToMove(direction: Direction, view: View): Boolean = {
        view.objectAt(direction.toRelativePosition) match {
          case Some(Empty) => true
          case Some(go: GameObject) => go.isGood
          case None => false
        }
      }

      def closestDirectionTo(direction: Direction, view: View): Option[Direction] = {
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

      def pathToFood: Option[BotCommand] = for {
        nearestFood <- nearestFoodOp
        shortestPath <- shortestPathTo(nearestFood, view)
        firstMove <- shortestPath.headOption
      } yield {
        Move(firstMove) + Status("FOOD!")
      }

      def moveTowardFood: Option[BotCommand] = for {
        nearestFood <- nearestFoodOp
        foodDirection <- nearestFood.direction
        moveDirection <- closestDirectionTo(foodDirection, view)
      } yield {
        Move(moveDirection) + Status("food?")
      }

      def runFromEnemy: Option[BotCommand] = for {
        nearestEnemy <- view.nearest(obj => Snorg == obj || EnemySlave == obj || EnemyBot == obj)
        enemyDirection <- nearestEnemy.direction
        runDirection <- closestDirectionTo(enemyDirection.reverse, view)
      } yield {
        Move(runDirection) + Status("Run Away!")
      }

      def stumped: Option[BotCommand] = {
        val randomDir = Direction.randomDirection
        for {
          moveDirection <- closestDirectionTo(randomDir, view)
        } yield {
          Move(moveDirection) + Status("???")
        }
      }

      pathToFood orElse moveTowardFood orElse runFromEnemy orElse stumped
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
}
