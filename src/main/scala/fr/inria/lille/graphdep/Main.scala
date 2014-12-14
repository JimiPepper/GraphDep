package fr.inria.lille.graphdep

import java.util.Date

import fr.inria.lille.graphdep.svg._
import scala.xml.XML


/**
 * Created by Jiraya on 13/12/2014.
 */
object Main extends App {
  val newDependencyColor =  RGB(0, 0, 255)
  val deleteDependencyColor = RGB(255, 0, 0)
  val spaceUnitDay = 25 // pixels
  val unitGraph = 10 // pixels
  var startX = 10 // pixels
  var maxY = 0 // pixel
  var minY = 0 // pixel
  var currentX = 0

  //lazy val dateFormat = new java.text.SimpleDateFormat("dd-MM-yyyy")
  def getDate(date : Array[Int]) = { new Date(date(0), date(1), date(2)) }
  val DAY_IN_MILLIS = 1000 * 60 * 60 * 24
  var lastDateCommit, currentCommitDate : Date = null
  var nbDays : Int = 0

  val svgElements = List[SVGElement]()

  lazy val xmlFile = XML.loadFile(args(0))
  lazy val commits = xmlFile \\ "analysis" \\ "history" \\ "commit"

  val SVGDoc = commits map { commit =>
    currentCommitDate = getDate((commit \\ "date").head.text.split("-").map(_.toInt))

    if(lastDateCommit == null) lastDateCommit = currentCommitDate

    nbDays = ((currentCommitDate.getTime() - lastDateCommit.getTime())/ DAY_IN_MILLIS ).toInt
    startX += nbDays * (spaceUnitDay + 0)
    lastDateCommit = currentCommitDate
    circle(startX, 10, 5)
  }

  export(SVGDoc)
}
