package fr.inria.lille.graphdep.svg

/**
 * Created by Jiraya on 13/12/2014.
 */
trait SVGStroking {
  def strokeColor(color : ColorSystem) : Unit
  def strokeWidth(width : Int) : Unit
  def strokeOpacity(value : Double) : Unit
}
