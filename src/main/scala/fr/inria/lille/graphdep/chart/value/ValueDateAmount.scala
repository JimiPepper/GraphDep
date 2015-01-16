package fr.inria.lille.graphdep.chart.value

import org.joda.time.DateTime

/**
 * @author Romain Philippon
 */
class ValueDateAmount(val x : DateTime, val y : Int) extends Value {
  def isPositiveX : Boolean = { true }

  def isPositiveY : Boolean = {
    if(y > 0) {
      true
    }
    else {
      false
    }
  }
}

object ValueDateAmount {
  def apply(date : DateTime, amount : Int) : ValueDateAmount = {
    new ValueDateAmount(date, amount)
  }
}
