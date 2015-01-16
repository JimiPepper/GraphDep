package fr.inria.lille.graphdep

import fr.inria.lille.graphdep.chart.kind.{GraphCommitDependency, GraphTimeAmount}
import fr.inria.lille.graphdep.chart.value.ValueCommitDependency
import fr.inria.lille.graphdep.svg._
import org.joda.time.{LocalDate, DateTime, Days}

import scala.math.log10

case class DrawingCursor(xStart : Int, yStart : Int) {
  var x : Int = xStart
  var y : Int = yStart
}
case class DrawingSpace(x1 : Int, y1 : Int, x2 : Int, y2 : Int)
case class Dimension(width : Int, height : Int)

/**
 * @author Romain Philippon
 */
package object util {
  val SVGDOCUMENT_MARGIN : Int = 10

  def drawGraph(dimension : Dimension, graph : GraphTimeAmount) : SVGDocument = {
    /* COLORS USED TO DRAW THIS GRAPH */
    val positiveValueColor =  HexaRGB("91D78D")
    val negativeValueColor = RGB(255, 0, 0)
    val unitValueColor = HexaRGB("CCCCCC")

    val svgDocument = document(dimension.width, dimension.height)

    /* GET COORDINATE NAME VALUE */
    val YUnits = graph.listUnitY
    val XUnits = graph.listUnitX

    val splitYCoordinates = YUnits.splitAt(YUnits.indexOf(1.0))
    val positiveYUnit = splitYCoordinates._2.tail.reverse.tail.reverse
    val negativeUnit = splitYCoordinates._1.tail.reverse
    val maxY = YUnits.reverse.head.toInt.toString
    val minY = YUnits.reverse.reverse.head.toInt.toString

    val startX = { // tell the longest y coordinate name
      if(minY.toString.length > maxY.toString.length) {
        minY.toString.length
      }
      else {
        maxY.toString.length
      }
    } * 12 // size of a character

    var nbDays = Days.daysBetween(graph.getMinX, graph.getMaxX).getDays // number of days between the first and last commit

    /* DEFINITION OF SPACE WHERE TO DRAW */
    val cursor = DrawingCursor(0, 0) // cursor

    val headerGraph = DrawingSpace(SVGDOCUMENT_MARGIN, SVGDOCUMENT_MARGIN, dimension.width - SVGDOCUMENT_MARGIN, 40) // top
    val yCoordinateGraph = DrawingSpace(SVGDOCUMENT_MARGIN, headerGraph.y2 + 14, startX, dimension.height - SVGDOCUMENT_MARGIN - 12) // left
    val xCoordinateTopGraph = DrawingSpace(yCoordinateGraph.x2, headerGraph.y2, dimension.width - SVGDOCUMENT_MARGIN, headerGraph.y2 + 12)
    val xCoordinateBottomGraph = DrawingSpace(yCoordinateGraph.x2, dimension.height - SVGDOCUMENT_MARGIN - 12, dimension.width - SVGDOCUMENT_MARGIN, dimension.height - SVGDOCUMENT_MARGIN)
    val graphDrawingSpace = DrawingSpace(yCoordinateGraph.x2, xCoordinateTopGraph.y2, dimension.width - SVGDOCUMENT_MARGIN, xCoordinateBottomGraph.y1 - 12)

    graph.unitX = (graphDrawingSpace.x2 - graphDrawingSpace.x1).toDouble / nbDays.toDouble
    graph.unitY = (graphDrawingSpace.y2 - graphDrawingSpace.y1).toDouble / (graph.getMaxY + graph.getMinY * -1)

    val middleOrdinate = (graph.getMaxY * graph.unitY).toInt + graphDrawingSpace.y1

    val values : List[(DateTime, Double)] = graph.getValues
    val startDate : DateTime = graph.getMinX

    /* DRAW HEADER */
    cursor.x = headerGraph.x1
    cursor.y = headerGraph.y1

    svgDocument.append(text(graph.name, cursor.x, cursor.y))
    svgDocument.append(text("x :"+ graph.xName +", y : "+ graph.yName, cursor.x, cursor.y + 14))

    /* DRAW Y-COORDINATE UNIT */
    cursor.x = yCoordinateGraph.x1

    // positive
    cursor.y = middleOrdinate
    positiveYUnit.foreach{ value =>
      cursor.y -= graph.unitY.toInt
      svgDocument.append(text(value.toInt.toString, cursor.x, cursor.y))
    }

    // negative
    cursor.y = middleOrdinate
    negativeUnit.foreach { value =>
      cursor.y += graph.unitY.toInt
      svgDocument.append(text(value.toInt.toString, cursor.x, cursor.y))
    }

    // max and min value
    svgDocument.append(text(maxY, cursor.x, yCoordinateGraph.y1)) // max
    svgDocument.append(text(minY, SVGDOCUMENT_MARGIN, yCoordinateGraph.y2 - 12)) // min

    // y-origin
    svgDocument.append(text("1", cursor.x, middleOrdinate))

    /* DRAW Y-UNIT LINES */
    // positive
    cursor.y = middleOrdinate
    positiveYUnit.foreach{ value =>
      cursor.y -= graph.unitY.toInt
      svgDocument.append(line(graphDrawingSpace.x1, cursor.y, graphDrawingSpace.x2, cursor.y).strokeColor(unitValueColor))
    }

    // negative
    cursor.y = middleOrdinate
    negativeUnit.foreach { value =>
      cursor.y += graph.unitY.toInt
      svgDocument.append(line(graphDrawingSpace.x1, cursor.y, graphDrawingSpace.x2, cursor.y).strokeColor(unitValueColor))
    }

    /* DRAW X-COORDINATE UNIT */
    XUnits.foreach{ date =>
      nbDays = Days.daysBetween(startDate, date).getDays
      cursor.x = xCoordinateTopGraph.x1 + (nbDays * graph.unitX).toInt
      svgDocument.append(line(cursor.x, graphDrawingSpace.y1, cursor.x, graphDrawingSpace.y2).strokeColor(RGB(200, 200, 200)))

      val dateString = date.getMonthOfYear +"/"+ date.getYear
      svgDocument.append(text(dateString, cursor.x, xCoordinateTopGraph.y1))
      svgDocument.append(text(dateString, cursor.x, xCoordinateBottomGraph.y1))
    }

    /* DRAW VALUES */
    values.foreach { value =>
      val commitDate : DateTime = value._1
      nbDays = Days.daysBetween(startDate, commitDate).getDays

      cursor.x = graphDrawingSpace.x1 + (nbDays.toDouble * graph.unitX).toInt
      cursor.y = middleOrdinate + (value._2 * graph.unitY * -1).toInt

      if(value._2 > 0) {
        svgDocument.append(line(cursor.x, middleOrdinate, cursor.x, cursor.y).strokeColor(positiveValueColor))
      }
      else {
        svgDocument.append(line(cursor.x, middleOrdinate, cursor.x, cursor.y).strokeColor(negativeValueColor))
      }
    }

    svgDocument.prepend(line(graphDrawingSpace.x1, middleOrdinate, graphDrawingSpace.x2, middleOrdinate)) // x-coordinate line
    svgDocument
  }

  def drawGraph(dimension : Dimension, graph : GraphCommitDependency) : SVGDocument = {
    /* COLORS USED TO DRAW THIS GRAPH */
    val positiveValueColor =  HexaRGB("78BF75")
    val unitValueColor = HexaRGB("CCCCCC")

    println(graph.listUnitX)

    val svgDocument = document(dimension.width, dimension.height)

    /* GET COORDINATE NAME VALUE */
    val YUnits = graph.listUnitY
    val XUnits = graph.listUnitX

    /* DEFINITION OF SPACE WHERE TO DRAW */
    val cursor = DrawingCursor(0, 0) // cursor

    val minY = graph getMinY
    val maxY = graph getMaxY

    val startX = {
      if(minY.toString.length > maxY.toString.length) {
        minY.toString.length
      }
      else {
        maxY.toString.length
      }
    } * 12

    val headerGraph = DrawingSpace(SVGDOCUMENT_MARGIN, SVGDOCUMENT_MARGIN, dimension.width - SVGDOCUMENT_MARGIN, 40) // top
    val yCoordinateGraph = DrawingSpace(SVGDOCUMENT_MARGIN, headerGraph.y2 + 14, startX, dimension.height - SVGDOCUMENT_MARGIN - 12) // left
    val xCoordinateGraph = DrawingSpace(yCoordinateGraph.x2, dimension.height - SVGDOCUMENT_MARGIN - 12, dimension.width - SVGDOCUMENT_MARGIN, dimension.height - SVGDOCUMENT_MARGIN)
    val graphDrawingSpace = DrawingSpace(yCoordinateGraph.x2, headerGraph.y2, dimension.width - SVGDOCUMENT_MARGIN, xCoordinateGraph.y1 - 12)

    graph.unitX = (graphDrawingSpace.x2 - graphDrawingSpace.x1).toDouble / graph.getMaxX.toDouble
    graph.unitY = (graphDrawingSpace.y2 - graphDrawingSpace.y1).toDouble / log10(maxY)

    println("unitX : "+ graph.unitX)

    val middleOrdinate = (log10(graph.getMaxY) * graph.unitY).toInt + graphDrawingSpace.y1

    val values : List[ValueCommitDependency] = graph.getValues

    /* DRAW HEADER */
    cursor.x = headerGraph.x1
    cursor.y = headerGraph.y1

    svgDocument.append(text(graph.name, cursor.x, cursor.y))
    svgDocument.append(text("x :"+ graph.xName +", y : "+ graph.yName, cursor.x, cursor.y + 14))

    /* DRAW Y-COORDINATE UNIT */
    cursor.x = yCoordinateGraph.x1
    graph.listUnitY.foreach { unit =>
      cursor.y = middleOrdinate - (log10(unit) * graph.unitY).toInt
      svgDocument.append(text(unit.toString, cursor.x, cursor.y))
    }

    // y-origin
    svgDocument.append(text("1", cursor.x, middleOrdinate))

    /* DRAW Y-UNIT LINES */
    cursor.y = middleOrdinate
    graph.listUnitY.foreach{ unit =>
      cursor.y = middleOrdinate - (log10(unit) * graph.unitY).toInt
      svgDocument.append(line(graphDrawingSpace.x1, cursor.y, graphDrawingSpace.x2, cursor.y).strokeColor(unitValueColor))
    }

    /* DRAW X-COORDINATE UNIT */
    graph.listUnitX.foreach{ unit =>
      cursor.x = xCoordinateGraph.x1 + (unit.toDouble * graph.unitX).toInt
      svgDocument.append(text(unit.toString, cursor.x, xCoordinateGraph.y1))
    }

    /* DRAW VALUES */
    values.foreach { value =>
      val y = log10(value.y)
      cursor.x = graphDrawingSpace.x1 + (value.x * graph.unitX).toInt
      cursor.y = middleOrdinate + (y * graph.unitY * -1).toInt

      svgDocument.append(line(cursor.x, middleOrdinate, cursor.x, cursor.y).strokeColor(positiveValueColor))
    }

    svgDocument.prepend(line(graphDrawingSpace.x1, middleOrdinate, graphDrawingSpace.x2, middleOrdinate)) // x-coordinate line
    svgDocument
  }

  def sameDate(date1 : DateTime, date2 : DateTime) : Boolean = {
    val simpleDate1 : LocalDate = new LocalDate(date1.getYear, date1.getMonthOfYear, date1.getDayOfMonth)
    val simpleDate2 : LocalDate = new LocalDate(date2.getYear, date2.getMonthOfYear, date2.getDayOfMonth)

    simpleDate1.equals(simpleDate2)
  }
}
