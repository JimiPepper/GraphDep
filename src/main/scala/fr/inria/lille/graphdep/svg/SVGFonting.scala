package fr.inria.lille.graphdep.svg

import fr.inria.lille.graphdep.SVGElement

/**
 * Created by Jiraya on 13/12/2014.
 */
trait SVGFonting {
  def size(size : String) : SVGElement
  def weight(weight : String) : SVGElement
}
