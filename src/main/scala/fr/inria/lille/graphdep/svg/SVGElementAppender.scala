package fr.inria.lille.graphdep.svg

import fr.inria.lille.graphdep.{SVGText, SVGCircle, SVGRectangle, SVGLine}

/**
 * Created by Jiraya on 13/12/2014.
 */

trait SVGElementAppender {
  def circle(x : Int, y : Int, radius : Int) : SVGCircle
  def rec(width: Int, height: Int, x : Int, y : Int) : SVGRectangle
  def line(x1 : Int, y1 : Int, x2 : Int, y2 : Int) : SVGLine
  def text(content: String, x : Int, y : Int) : SVGText
}
