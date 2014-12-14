package fr.inria.lille.graphdep.svg

import fr.inria.lille.graphdep.SVGElement

/**
 * Created by Jiraya on 13/12/2014.
 */
trait SVGStroking {
  def strokeColor(color : ColorSystem) : SVGElement
  def strokeWidth(width : Int) : SVGElement
  def strokeOpacity(value : Double) : SVGElement
}
