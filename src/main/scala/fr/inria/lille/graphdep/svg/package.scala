package fr.inria.lille.graphdep

import fr.inria.lille.graphdep.svg.{SVGColor, SVGStroking, RGB, ColorSystem}

/**
 * @author : Romain Philippon
 */
sealed abstract class SVGElement
case class SVGCircle(x: Int, y : Int, fillColor : ColorSystem = RGB(255, 255, 255), stroke : SVGStroke = new SVGStroke) extends SVGElement with SVGColor with SVGStroking
case class SVGRectangle(width: Int, height: Int, x: Int, y : Int, fillColor : ColorSystem = RGB(255, 255, 255), stroke : SVGStroke = new SVGStroke) extends SVGElement with SVGColor with SVGStroking
case class SVGLine(x1 : Int, y1 : Int, x2 : Int, y2 : Int, stroke : SVGStroke = new SVGStroke) extends SVGElement with SVGStroking
case class SVGText(content: String, x : Int, y : Int, color : ColorSystem = RGB(255, 255, 255), font : SVGFont = new SVGFont) extends SVGElement with with SVGColor with SVGStroking
case class SVGTitle(title : String)

sealed abstract class SVGAttribute
case class SVGStroke(color : ColorSystem = RGB(255, 255, 255), width : Int = 1, opacity : Double = 1.0) extends SVGAttribute
case class SVGFont(size: String = "0px", weight: String = "normal")


package object svg extends SVGElementAppender {
  override def circle(x : Int, y : Int) : SVGCircle = {
    SVGCircle(x, y)
  }

  override def rec(width: Int, height: Int, x : Int, y : Int) : SVGRectangle = {
    SVGRectangle(width, height, x, y)
  }

  override def line(x1 : Int, y1 : Int, x2 : Int, y2 : Int) : SVGLine = {
    SVGLine(x1, y1, x2, y2)
  }

  override def text(content: String, x : Int, y : Int) : SVGText = {
    SVGText(content, x, y)
  }

  def export(list : Seq[SVGElement]) : Unit = {

  }
}
