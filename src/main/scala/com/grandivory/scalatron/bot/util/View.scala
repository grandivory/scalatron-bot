package com.grandivory.scalatron.bot.util

import PositionVectorConversions._

/**
  * A representation of everything that a bot can see
  */
class View private(val distance: Int, val objectsInView: Map[RelativePosition, ViewObject]) extends Equals {
  def canSeeSnorgs: Boolean = objectsInView.values.exists(Snorg == _)
  def canSeeFluppets: Boolean = objectsInView.values.exists(Fluppet == _)
  def canSeeZugars: Boolean = objectsInView.values.exists(Zugar == _)
  def canSeeToxifera: Boolean = objectsInView.values.exists(Toxifera == _)
  def nearestGameObject: Option[(RelativePosition, GameObject)] = {
    // The scala compiler can't seem to figure out the type of this simple partial function unless I explicitly break
    // it out and explicitly assign it.
    val conversionMethod: PartialFunction[(RelativePosition, ViewObject), (RelativePosition, GameObject)] = {
      case entry@(_, _:GameObject) => entry.asInstanceOf[(RelativePosition, GameObject)]
    }

    objectsInView.toList.collect(conversionMethod).sortWith(_._1 < _._1).find(_._2 != MasterBot)
  }

  def canSee(position: RelativePosition): Boolean = objectAt(position).nonEmpty
  def objectAt(position: RelativePosition): Option[ViewObject] = objectsInView.get(position)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[View]

  override def equals(other: Any): Boolean = other match {
    case that: View => that.canEqual(this) &&
      that.distance == this.distance &&
      that.objectsInView == this.objectsInView
    case _ => false
  }

  override def toString: String = s"View@$hashCode"
}

object View {
  /**
    * Given a view string, as defined by Scalatron, return a View object that can be queried
    *
    * @param viewString A string containing the characters "?_WMmSsPpBb", representing a square view
    */
  def apply(viewString: String): View = {
    val viewSize: Int = Math.sqrt(viewString.length).toInt
    val viewDistance: Int = (viewSize - 1) / 2
    val rows: Iterator[String] = viewString.grouped(viewSize)

    // Construct a map of objects at their positions based on the input string
    val viewObjects: Map[RelativePosition, ViewObject] = rows.zipWithIndex.foldRight (Map.empty[RelativePosition, ViewObject]) {
      case ((rowView: String, row: Int), outerAcc: Map[RelativePosition, ViewObject]) =>
        rowView.zipWithIndex.foldRight(outerAcc) {
          case ((cell: Char, column: Int), innerAcc: Map[RelativePosition, ViewObject]) =>
            val position = RelativePosition((column - viewDistance).right, (row - viewDistance).down)
            cell match {
              case '_' => innerAcc + (position -> ViewObject.Empty)
              case 'W' => innerAcc + (position -> Wall)
              case 'M' => innerAcc + (position -> MasterBot)
              case 'm' => innerAcc + (position -> EnemyBot)
              case 'S' => innerAcc + (position -> Slave)
              case 's' => innerAcc + (position -> EnemySlave)
              case 'P' => innerAcc + (position -> Zugar)
              case 'p' => innerAcc + (position -> Toxifera)
              case 'B' => innerAcc + (position -> Fluppet)
              case 'b' => innerAcc + (position -> Snorg)
              case _ => innerAcc
            }
        }
    }

    new View(viewDistance,viewObjects)
  }
}
