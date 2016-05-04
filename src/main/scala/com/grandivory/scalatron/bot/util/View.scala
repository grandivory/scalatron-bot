package com.grandivory.scalatron.bot.util

/**
  * A representation of everything that a bot can see
  */
trait View extends Equals {
  def distance: Int
  def objectsInView: Map[RelativePosition, ViewObject]

  val canSeeSnorgs: Boolean = objectsInView.values.exists(Snorg == _)
  val canSeeFluppets: Boolean = objectsInView.values.exists(Fluppet == _)
  val canSeeZugars: Boolean = objectsInView.values.exists(Zugar == _)
  val canSeeToxifera: Boolean = objectsInView.values.exists(Toxifera == _)
  val nearestObject: Option[GameObject] = objectsInView.toList.filter(_._2.isInstanceOf[GameObject])
    .sortWith(_._1 < _._1).drop(1).headOption.map {
    case (_, gameObject: GameObject) => gameObject
  }

  def canSee(position: RelativePosition): Boolean = objectAt(position).nonEmpty
  def objectAt(position: RelativePosition): Option[ViewObject] = objectsInView.get(position)
  def spaceHas[T <: GameObject](position: RelativePosition): Option[Boolean] = objectAt(position).map(_.isInstanceOf[T])

  override def canEqual(other: Any): Boolean = other.isInstanceOf[View]

  override def equals(other: Any): Boolean = other match {
    case that: View => that.canEqual(this) &&
      that.distance == this.distance &&
      that.objectsInView == this.objectsInView
    case _ => false
  }
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
            val position = RelativePosition(column - viewDistance, row - viewDistance)
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
            }
        }
    }

    new View {
      val distance = viewDistance
      val objectsInView = viewObjects
    }
  }
}
