package fr.inria.lille.graphdep

/**
 * Created by Jiraya on 13/12/2014.
 */
sealed abstract class SVGElement
case class SVGCircle(x: Int, y : Int) extends SVGElement
case class SVGRectangle(width: Int, height: Int, x: Int, y : Int) extends SVGElement
case class SVGLine(x1 : Int, y1 : Int, x2 : Int, y2 : Int) extends SVGElement
case class SVGText(content: String, x : Int, y : Int) extends SVGElement

sealed abstract class ColorSystem
case class RGB(red : Int, green : Int, blue : Int) extends ColorSystem {
  if(red < 0  || red > 255) {
    throw new IllegalArgumentException("RGB values must be between 0 and 255 (red : "+ red +")")
  }

  if(green < 0  || green > 255) {
    throw new IllegalArgumentException("RGB values must be between 0 and 255 (green : "+ green +")")
  }

  if(blue < 0  || blue > 255) {
    throw new IllegalArgumentException("RGB values must be between 0 and 255 (blue : "+ blue +")")
  }
}

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
