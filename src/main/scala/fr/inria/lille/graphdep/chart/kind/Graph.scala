package fr.inria.lille.graphdep.chart.kind

import fr.inria.lille.graphdep.chart.value.Value

/**
 * @author Romain Philippon
 */
trait Graph[A, B] {
  /*
  def add(value : Value) : Unit

  def remove(value : Value) : Unit
  */

  def size : Int

  /*
  def getValues : List[(A, B)]
  */

  def getMaxX : A

  def getMinX : A

  def getMaxY : B

  def getMinY : B

  def listUnitX : List[A]

  def listUnitY : List[B]
}
