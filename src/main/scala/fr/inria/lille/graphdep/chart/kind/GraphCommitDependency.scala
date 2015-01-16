package fr.inria.lille.graphdep.chart.kind

import fr.inria.lille.graphdep.chart.value.ValueCommitDependency

import scala.math.log10
import scala.math.pow

/**
 * @author Romain Philippon
 */
class GraphCommitDependency(graphName : String, xCoordinateName : String, yCoordinateName : String) extends Graph[Int, Int] {
  private var valuesList : List[ValueCommitDependency] = Nil
  var unitX : Double = 10
  var unitY : Double = 10
  var name : String = graphName
  var xName : String = xCoordinateName
  var yName : String = yCoordinateName

  def add(value: ValueCommitDependency): Unit = {
    valuesList = value :: valuesList
  }

  def remove(value: ValueCommitDependency): Unit = {
    valuesList = valuesList diff List(value)
  }

  def getMaxX: Int = {
    valuesList.reduceLeft((value1, value2) => if (value1.x > value2.x) value1 else value2).x
  }

  override def getMinX: Int = {
    valuesList.reduceLeft((value1, value2) => if (value1.x < value2.x) value1 else value2).x
  }

  override def listUnitX: List[Int] = {
    val unit = pow(10, log10(getMaxX).toInt).toInt
    List.range(0, getMaxX, unit) :+ getMaxX
  }

  override def size: Int = {
    valuesList size
  }

  override def getMinY: Int = {
    valuesList.reduceLeft((value1, value2) => if (value1.y < value2.y) value1 else value2).y
  }

  def getValues: List[ValueCommitDependency] = {
    valuesList
  }

  override def getMaxY: Int = {
    valuesList.reduceLeft((value1, value2) => if (value1.y > value2.y) value1 else value2).y
  }

  override def listUnitY: List[Int] = {
    var start = 10
    val maxY = getMaxY
    var list : List[Int] = Nil

    while(start < maxY) {
      list = list :+ start
      start *= 10
    }

    list :+ maxY
  }
}

object GraphCommitDependency {
  def apply(graphName : String, xCoordinateName : String, yCoordinateName : String) : GraphCommitDependency = {
    new GraphCommitDependency(graphName, xCoordinateName, yCoordinateName)
  }
}
