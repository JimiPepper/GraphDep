package fr.inria.lille.graphdep

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

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

  var currentCommitDate, commitDate, lastCommitDate : Date = null // contains the commit date of the current visited commit node while parsing
  var parseInformation = List[(Date, Int, Int)]() // tuple <=> (commitDate, nbNewDependency, nbDeletedDependency)

  /* PARSING INITIALIZATION */
  lazy val xmlFile = XML.loadFile(args(0))
  lazy val commits = xmlFile \\ "analysis" \\ "history" \\ "commit"
  currentCommitDate = getDate(((commits head) \\ "date") text) // get first commit date
  parseInformation = (currentCommitDate, 0, 0) :: parseInformation
  var nbNewAddedDependency, nbNewDeletedDependency : Int = 0

  /* PARSE XML RESULT FILE TO COLLECT INFORMATION DAY BY DAY */
  commits foreach { commit =>
    commitDate = getDate((commit \\ "date").text)

    if(sameDate(currentCommitDate, commitDate)) {
      /* UPDATE INFORMATION FOR THE DAY */
      nbNewAddedDependency = parseInformation.last._2 + (commit \\ "dependencies" \\ "added-dependency").size
      nbNewDeletedDependency = parseInformation.last._3 + (commit \\ "dependencies" \\ "deleted-dependency").size
      updateCommitInformation(nbNewAddedDependency, nbNewDeletedDependency)
    }
    else {
      /* ADD NEW COMMIT DATE INFORMATION */
      currentCommitDate = getDate((commit \\ "date") text)
      nbNewAddedDependency = (commit \\ "dependencies" \\ "added-dependency").size
      nbNewDeletedDependency = (commit \\ "dependencies" \\ "deleted-dependency").size
      addCommitInformation(currentCommitDate, nbNewAddedDependency, nbNewDeletedDependency)
    }
  }

  /* GENERATE SVG FILE */
  val maxAddedDependency = parseInformation.maxBy(_._2)
  val maxDeletedDependency = parseInformation.maxBy(_._3)

  val widthSVG = (20 + parseInformation.size * spaceUnitDay)
  val heightSVG = (20 + maxAddedDependency._2 + unitGraph + + maxDeletedDependency._3 + unitGraph)
  var middleOrdinate = heightSVG / 2

  val chronoLine = line(10, middleOrdinate, widthSVG - 10, middleOrdinate)
  currentX = startX
  lastCommitDate = parseInformation.head._1
  var addDepLine, delDepLine, commitDateCirc : SVGElement = null

  parseInformation foreach { case (date : Date, nbAddedDependency : Int, nbDeletedDependencies : Int) =>
    /* CREATE SVG ELEMENTS */
    if(nbAddedDependency == 0 && nbDeletedDependencies == 0) // if no dependency changes detected
      commitDateCirc = circle(currentX, middleOrdinate, 5).fillColor(RGB(255, 255, 255))
    else {
      commitDateCirc = circle(currentX, middleOrdinate, 5)
      addDepLine = line(currentX, middleOrdinate, currentX, middleOrdinate - nbAddedDependency * unitGraph).strokeColor(newDependencyColor)
      delDepLine = line(currentX, middleOrdinate, currentX, middleOrdinate + nbDeletedDependencies * unitGraph).strokeColor(deleteDependencyColor)
    }

    /* APPENDS SVG ELEMENTS */
    svgElements = svgElements.:+(addDepLine)
    svgElements = svgElements.:+(delDepLine)
    svgElements = svgElements.:+(commitDateCirc)

    nbDays = ((date.getTime() - lastCommitDate.getTime())/ DAY_IN_MILLIS ).toInt
    currentX += nbDays * spaceUnitDay
    lastCommitDate = date
  }

  svgElements = chronoLine :: svgElements // add chrono line

  export(widthSVG, heightSVG, svgElements)

  /**
   * Returns an Date object from a string representation
   * @param date
   * @return a date object
   */
  def getDate(date : String) = { val format = new SimpleDateFormat("yyyy-MM-dd") ; format.parse(date) }

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
  def addCommitInformation(date : Date, nbAddedDependency : Int, nbDeletedDependency : Int) : Unit = {
    parseInformation = parseInformation.:+(currentCommitDate, nbNewAddedDependency, nbNewDeletedDependency)
  }

  /**
   * Checks if two dates are the same according the year, month and day of the year
   * @param date1 is the first date
   * @param date2 is the second date that must be compare to date1
   * @return true if the two dates are the same, else otherwise
   */
  def sameDate(date1 : Date, date2 : Date) : Boolean = {
    val cal1 : Calendar = Calendar.getInstance()
    val cal2 : Calendar = Calendar.getInstance()

    cal1.setTime(date1)
    cal2.setTime(date2)

    cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal1.get(Calendar.MONTH) == cal2.get(Calendar.MONTH) && cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR)
  }
}