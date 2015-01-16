package fr.inria.lille.graphdep.chart.kind

import fr.inria.lille.graphdep.chart.value.{Value, ValueDateAmount}
import org.joda.time.{DateTime, Days}

import scala.math.log10
import scala.math.pow

/**
 * @author Romain Philippon
 */
class GraphTimeAmount(graphName : String, xCoordinateName : String, yCoordinateName : String) extends Graph[DateTime, Double] {
  private var valuesList : List[ValueDateAmount] = Nil
  var unitX : Double = 10
  var unitY : Double = 10
  var name : String = graphName
  var xName : String = xCoordinateName
  var yName : String = yCoordinateName

  def add(value : ValueDateAmount) : Unit = {
    valuesList = value :: valuesList
  }

  def remove(value : ValueDateAmount) : Unit = {
    valuesList = valuesList diff List(value)
  }

  override def size : Int = {
    valuesList length
  }

  def getValues : List[(DateTime, Double)] = {
    var nbDependencies : Double = 0

    valuesList.map { value =>
      if(value.isPositiveY) {
        nbDependencies = log10(value.y.toDouble)
      }
      else {
        nbDependencies = log10(value.y.toDouble * -1) * -1
      }

      (value.x, nbDependencies)
    }
  }

  override def getMaxX : DateTime = {
    valuesList.reduceLeft((value1, value2) => if (value1.x.isAfter(value2.x.getMillis)) value1 else value2).x
  }

  override def getMinX : DateTime = {
    valuesList.reduceLeft((value1, value2) => if (value1.x.isBefore(value2.x.getMillis)) value1 else value2).x
  }

  override def getMaxY : Double = {
    val value = valuesList.reduceLeft((value1, value2) => if (value1.y > value2.y) value1 else value2).y
    log10(value.toDouble)
  }

  override def getMinY : Double = {
    val value = valuesList.reduceLeft((value1, value2) => if (value1.y < value2.y) value1 else value2).y
    log10(value * -1) * -1
  }

  override def listUnitX : List[DateTime] = {
    var list : List[DateTime] = Nil

    val minDate = getMinX
    val maxDate = getMaxX
    val startDate = minDate
    var currentDate : DateTime = null

    val nbDays = Days.daysBetween(minDate, maxDate).getDays

    for(inc <- 0 until nbDays by 15) {
      currentDate = startDate.plusDays(inc)
      currentDate = new DateTime(currentDate.getYear, 1, 1, 0, 0)

      if(!list.contains(currentDate)) {
        list = currentDate :: list
      }
    }

    list
  }

  override def listUnitY : List[Double] = {
    val minY = pow(10, getMinY * -1) * -1
    val maxY = pow(10, getMaxY)

    var positiveYCoordinate : List[Double] = Nil
    var negativeYCoordinate : List[Double] = Nil

    /* POSITIVE Y-COORDINATE */
    var currentYCoordinate : Double = 10
    while(currentYCoordinate < maxY) {
      positiveYCoordinate = positiveYCoordinate :+ currentYCoordinate
      currentYCoordinate *= 10

    }

    positiveYCoordinate = positiveYCoordinate :+ maxY

    /* NEGATIVE Y-COORDINATE */
    currentYCoordinate = -10
    while(currentYCoordinate > minY) {
      negativeYCoordinate = currentYCoordinate +: negativeYCoordinate
      currentYCoordinate *= 10
    }

    negativeYCoordinate = minY +: negativeYCoordinate

    negativeYCoordinate ::: List(1.0) ::: positiveYCoordinate
  }
}

object GraphTimeAmount {
  def apply(graphName : String, nameX : String, nameY : String) : GraphTimeAmount = { new GraphTimeAmount(graphName, nameX, nameY) }
}
