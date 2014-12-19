package fr.inria.lille.graphdep

import org.joda.time.format.DateTimeFormat
import org.joda.time.{LocalDate, Days}

import fr.inria.lille.graphdep.svg._
import scala.xml.XML


/**
 * Created by Jiraya on 13/12/2014.
 */
object Main extends App {
  /* VARIABLES TO GENERATE SVG FILE */
  val newDependencyColor =  RGB(0, 0, 255)
  val deleteDependencyColor = RGB(255, 0, 0)
  val spaceUnitDay = 25 // pixels
  val unitGraph = 5 // pixels
  var startX = 10 // pixels
  var maxY = 0 // pixel
  var minY = 0 // pixel
  var nbDays = 0
  var currentX = 0
  var svgElements = List[SVGElement]()
  val DAY_IN_MILLIS = 1000 * 60 * 60 * 24

  var currentCommitDate, commitDate, lastCommitDate : LocalDate = null // contains the commit date of the current visited commit node while parsing
  var parseInformation = List[(LocalDate, Int, Int)]() // tuple <=> (commitDate, nbNewDependency, nbDeletedDependency)

  /* PARSING INITIALIZATION */
  lazy val xmlFile = XML.loadFile(args(0))
  lazy val commits = xmlFile \\ "analysis" \\ "history" \\ "commit"
  currentCommitDate = LocalDate.parse(((commits head) \\ "date") text) // get first commit date
  parseInformation = (currentCommitDate, 0, 0) :: parseInformation
  var nbNewAddedDependency, nbNewDeletedDependency : Int = 0

  /* PARSE XML RESULT FILE TO COLLECT INFORMATION DAY BY DAY */
  commits foreach { commit =>
    commitDate = LocalDate.parse((commit \\ "date").text)

    if(sameDate(currentCommitDate, commitDate)) {
      /* UPDATE INFORMATION FOR THE DAY */
      nbNewAddedDependency = parseInformation.last._2 + (commit \\ "dependencies" \\ "added-dependency").size
      nbNewDeletedDependency = parseInformation.last._3 + (commit \\ "dependencies" \\ "deleted-dependency").size
      updateCommitInformation(nbNewAddedDependency, nbNewDeletedDependency)
    }
    else {
      /* ADD NEW COMMIT DATE INFORMATION */
      currentCommitDate = LocalDate.parse((commit \\ "date") text)
      nbNewAddedDependency = (commit \\ "dependencies" \\ "added-dependency").size
      nbNewDeletedDependency = (commit \\ "dependencies" \\ "deleted-dependency").size
      addCommitInformation(currentCommitDate, nbNewAddedDependency, nbNewDeletedDependency)
    }
  }

  /* GENERATE SVG FILE */
  val maxAddedDependency = parseInformation.maxBy(_._2)
  val maxDeletedDependency = parseInformation.maxBy(_._3)

  val heightSVG = 100 + (maxAddedDependency._2 + maxDeletedDependency._3) * unitGraph
  val widthSVG = 20 + Days.daysBetween(parseInformation.head._1, parseInformation.last._1).getDays * spaceUnitDay

  var middleOrdinate = 50 + maxAddedDependency._2 * unitGraph
  val chronoLine = line(10, middleOrdinate, widthSVG - 10, middleOrdinate)

  currentX = startX
  lastCommitDate = parseInformation.head._1
  var addDepLine, delDepLine, commitDateCirc, nbAddDeptext, nbDelDeptext : SVGElement = null

  parseInformation foreach { case (date : LocalDate, nbAddedDependency : Int, nbDeletedDependency : Int) =>
    /* MOVE VIRTUAL DRAWING CURSOR */
    nbDays = Days.daysBetween(lastCommitDate, date).getDays
    currentX += nbDays * spaceUnitDay
    lastCommitDate = date

    /* CREATE SVG ELEMENTS */
    if(nbAddedDependency == 0 && nbDeletedDependency == 0) { // if no dependency changes detected
      commitDateCirc = circle(currentX, middleOrdinate, 5).fillColor(RGB(255, 255, 255))
    }
    else {
      commitDateCirc = circle(currentX, middleOrdinate, 5)

      if (nbAddedDependency > 0) {
        nbAddDeptext = text("+" + nbAddedDependency, currentX, middleOrdinate - (nbAddedDependency * unitGraph) - 5).fillColor(newDependencyColor)
        addDepLine = line(currentX, middleOrdinate, currentX, middleOrdinate - (nbAddedDependency * unitGraph)).strokeColor(newDependencyColor)
        svgElements = svgElements.:+(addDepLine)
      }
      else {
        nbAddDeptext = text("0", currentX, middleOrdinate - (nbAddedDependency * unitGraph) - 5).fillColor(newDependencyColor)
      }

      if(nbDeletedDependency > 0) {
        nbDelDeptext = text("-" + nbDeletedDependency, currentX, middleOrdinate + (nbDeletedDependency * unitGraph) + 12 + 5).fillColor(deleteDependencyColor)
        delDepLine = line(currentX, middleOrdinate, currentX, middleOrdinate + (nbDeletedDependency * unitGraph)).strokeColor(deleteDependencyColor)
        svgElements = svgElements.:+(delDepLine)
      }
      else {
        nbDelDeptext = text("0", currentX, middleOrdinate + nbDeletedDependency * unitGraph + 12 + 5).fillColor(deleteDependencyColor)
      }

      // TODO : Make an object for each svg export data
      svgElements = svgElements.:+(nbAddDeptext)
      svgElements = svgElements.:+(nbDelDeptext)
    }

    /* APPENDS SVG ELEMENTS */

    svgElements = svgElements.:+(commitDateCirc)
  }

  svgElements = chronoLine :: svgElements // add chrono line

  export(widthSVG, heightSVG, svgElements)

  /**
   * Updates the the commit day information node in parseInformation variable
   * @param nbAddedDependency is the number of new added dependencies
   * @param nbDeletedDependency is the number of new deleted dependencies
   */
  def updateCommitInformation(nbAddedDependency : Int, nbDeletedDependency : Int) : Unit = {
    parseInformation = parseInformation.updated(parseInformation.size - 1, (currentCommitDate, nbAddedDependency, nbDeletedDependency))
  }

  /**
   * Adds a new a commit day information node in parseInformation variable
   * @param date is the new commit day date to register
   * @param nbAddedDependency is the number of new added dependencies
   * @param nbDeletedDependency is the number of new deleted dependencies
   */
  def addCommitInformation(date : LocalDate, nbAddedDependency : Int, nbDeletedDependency : Int) : Unit = {
    parseInformation = parseInformation.:+(currentCommitDate, nbNewAddedDependency, nbNewDeletedDependency)
  }

  /**
   * Checks if two dates are the same according the year, month and day of the year
   * @param date1 is the first date
   * @param date2 is the second date that must be compare to date1
   * @return true if the two dates are the same, else otherwise
   */
  def sameDate(date1 : LocalDate, date2 : LocalDate) : Boolean = { date1.equals(date2) }
}