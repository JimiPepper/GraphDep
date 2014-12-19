package fr.inria.lille.graphdep

import java.io.File

import fr.inria.lille.graphdep.svg._

import scala.xml.XML
import scala.xml.dtd.{PublicID, DocType}

/**
 * @author : Romain Philippon
 */
sealed abstract class SVGElement

case class SVGCircle(x: Int, y : Int, radius : Int, fillColor : ColorSystem = RGB(0, 0, 0), stroke : SVGStroke = new SVGStroke) extends SVGElement with SVGColor with SVGStroking {
  override def fillColor(color: ColorSystem): SVGElement = {
    color match {
      case RGB(red, green, blue) => SVGCircle(this.x, this.y, this.radius, color, this.stroke)
    }
  }

  override def strokeWidth(width: Int): SVGElement = {
    SVGCircle(this.x, this.y, this.radius, this.fillColor, SVGStroke(this.stroke.color, width, this.stroke.opacity))
  }

  override def strokeOpacity(opacity: Double): SVGElement = {
    SVGCircle(this.x, this.y, this.radius, this.fillColor, SVGStroke(this.stroke.color, this.stroke.width, opacity))
  }

  override def strokeColor(color: ColorSystem): SVGElement = {
    SVGCircle(this.x, this.y, this.radius, this.fillColor, SVGStroke(color, this.stroke.width, this.stroke.opacity))
  }
}

case class SVGRectangle(width: Int, height: Int, x: Int, y : Int, fillColor : ColorSystem = RGB(0, 0, 0), stroke : SVGStroke = new SVGStroke) extends SVGElement with SVGColor with SVGStroking {
  override def fillColor(color: ColorSystem): SVGElement = {
    color match {
      case RGB(red, green, blue) => SVGRectangle(this.width, this.height, this.x, this.y, color, this.stroke)
    }
  }

  override def strokeWidth(width: Int): SVGElement = {
    SVGRectangle(this.width, this.height, this.x, this.y, this.fillColor, SVGStroke(this.stroke.color, width, this.stroke.opacity))
  }

  override def strokeOpacity(opacity: Double): SVGElement = {
    SVGRectangle(this.width, this.height, this.x, this.y, this.fillColor, SVGStroke(this.stroke.color, this.stroke.width, opacity))
  }

  override def strokeColor(color: ColorSystem): SVGElement = {
    SVGRectangle(this.width, this.height, this.x, this.y, this.fillColor, SVGStroke(color, this.stroke.width, this.stroke.opacity))
  }
}

case class SVGLine(x1 : Int, y1 : Int, x2 : Int, y2 : Int, stroke : SVGStroke = new SVGStroke) extends SVGElement with SVGStroking {
  override def strokeColor(color: ColorSystem): SVGElement = {
    SVGLine(this.x1, this.y1, this.x2, this.y2, SVGStroke(color, this.stroke.width, this.stroke.opacity))
  }

  override def strokeWidth(width: Int): SVGElement = {
    SVGLine(this.x1, this.y1, this.x2, this.y2, SVGStroke(this.stroke.color, width, this.stroke.opacity))
  }

  override def strokeOpacity(opacity: Double): SVGElement = {
    SVGLine(this.x1, this.y1, this.x2, this.y2, SVGStroke(this.stroke.color, this.stroke.width, opacity))
  }
}

case class SVGText(content: String, x : Int, y : Int, color : ColorSystem = RGB(0, 0, 0), font : SVGFont = new SVGFont) extends SVGElement with SVGColor with SVGFonting {
  override def fillColor(color: ColorSystem): SVGElement = {
    color match {
      case RGB(red, green, blue) => SVGText(this.content, this.x, this.y, RGB(red, green, blue), this.font)
    }
  }

  override def size(size: String): SVGElement = {
    SVGText(this.content, this.x, this.y, this.color, SVGFont(size, this.font.weight))
  }

  override def weight(weight: String): SVGElement = {
    SVGText(this.content, this.x, this.y, this.color, SVGFont(this.font.size, weight))
  }
}
case class SVGTitle(title : String)

sealed abstract class SVGAttribute
case class SVGStroke(color : ColorSystem = RGB(0, 0, 0), width : Int = 1, opacity : Double = 1.0) extends SVGAttribute
case class SVGFont(size: String = "0px", weight: String = "normal")


package object svg extends SVGElementAppender {
  override def circle(x : Int, y : Int, radius : Int) : SVGCircle = {
    SVGCircle(x, y, radius)
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

  def export(width : Int, height : Int, list : Seq[SVGElement]) : Unit = {
    val doctype = DocType(
        "svg",
        PublicID(
          "-//W3C//DTD SVG 1.1//EN",
          "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"
        ),
        Nil

    )

    val xmlSVG = <svg width={width +"px"} height={height +"px"} version="1.1" xmlns="http://www.w3.org/2000/svg">{
      for (shape <- list) yield {
          shape match {
            case SVGCircle(x, y, radius, fillColor, stroke) => <circle cx={x toString} cy={y toString} r={radius toString} stroke={stroke.color.toString} stroke-width={stroke.width.toString} fill={fillColor toString} />
            case _ =>
          }
        }
      }</svg>

    XML.save(System.getProperty("user.home") + File.separator + "depgraph.svg", xmlSVG, "UTF-8", true, doctype)
  }
}
