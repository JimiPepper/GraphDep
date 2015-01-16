package fr.inria.lille.graphdep.chart.kind

import fr.inria.lille.graphdep.chart.value.{Value, ValueDateAmount}
import org.joda.time.{DateTime, Days}

import scala.math.log10
import scala.math.pow

/**
 * @author Romain Philippon
 */
class GraphTimeAmount(graphName : String, xCoordinateName : String, yCoordinateName : String) extends Graph[DateTime, Int] {
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
    valuesList.length
  }

  def getValues : List[ValueDateAmount] = {
    valuesList
  }

  def getMaxX : DateTime = {
    valuesList.reduceLeft((value1, value2) => if (value1.x.isAfter(value2.x.getMillis)) value1 else value2).x
  }

  def getMinX : DateTime = {
    valuesList.reduceLeft((value1, value2) => if (value1.x.isBefore(value2.x.getMillis)) value1 else value2).x
  }

  def getMaxY : Int = {
    valuesList.reduceLeft((value1, value2) => if (value1.y > value2.y) value1 else value2).y
  }

  def getMinY : Int = {
    valuesList.reduceLeft((value1, value2) => if (value1.y < value2.y) value1 else value2).y
  }

  def listUnitX : List[DateTime] = {
    val NUMBER_DAY_YEAR = 365
    val NUMBER_DAY_MONTH = 30

    var list : List[DateTime] = Nil

    val minDate = getMinX
    val maxDate = getMaxX
    val startDate = minDate
    var currentDate : DateTime = null

    val nbDays = Days.daysBetween(minDate, maxDate).getDays

    if(nbDays > NUMBER_DAY_YEAR) {
      for (inc <- 0 until nbDays by 15) {
        currentDate = startDate.plusDays(inc)
        currentDate = new DateTime(currentDate.getYear, 1, 1, 0, 0)

        if (!list.contains(currentDate)) {
          list = currentDate :: list
        }
      }
    }
    else if(nbDays > NUMBER_DAY_MONTH) {
      for (inc <- 0 until nbDays by 30) {
        currentDate = startDate.plusDays(inc)
        currentDate = new DateTime(currentDate.getYear, currentDate.getMonthOfYear, 1, 0, 0)

        if (!list.contains(currentDate)) {
          list = currentDate :: list
        }
      }
    }
    else {
      for (inc <- 0 until nbDays by 1) {
        currentDate = startDate.plusDays(inc)
        currentDate = new DateTime(currentDate.getYear, currentDate.getMonthOfYear, currentDate.getDayOfMonth, 0, 0)

        if (!list.contains(currentDate)) {
          list = currentDate :: list
        }
      }
    }

    list
  }

  def listUnitY : List[Int] = {
    val minY = getMinY
    val maxY = getMaxY

    var positiveYCoordinate : List[Int] = Nil
    var negativeYCoordinate : List[Int] = Nil

    /* POSITIVE Y-COORDINATE */
    var currentYCoordinate : Int = 10
    while(currentYCoordinate < maxY) {
      positiveYCoordinate = positiveYCoordinate :+ currentYCoordinate
      currentYCoordinate *= 10
    }

    positiveYCoordinate = positiveYCoordinate :+ getMaxY

    /* NEGATIVE Y-COORDINATE */
    currentYCoordinate = -10
    while(currentYCoordinate > minY) {
      negativeYCoordinate = currentYCoordinate +: negativeYCoordinate
      currentYCoordinate *= 10
    }

    negativeYCoordinate = getMinY +: negativeYCoordinate

    negativeYCoordinate ::: List(0) ::: positiveYCoordinate
  }
}

object GraphTimeAmount {
  def apply(graphName : String, nameX : String, nameY : String) : GraphTimeAmount = { new GraphTimeAmount(graphName, nameX, nameY) }
}
