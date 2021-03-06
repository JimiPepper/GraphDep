package fr.inria.lille.graphdep

import fr.inria.lille.graphdep.chart.kind.{GraphCommitDependency, GraphTimeAmount}
import fr.inria.lille.graphdep.chart.value.{ValueCommitDependency, ValueDateAmount}
import org.joda.time.format.{DateTimeFormatter, DateTimeFormat}
import org.joda.time.{LocalDate, DateTime}

import fr.inria.lille.graphdep.svg._
import fr.inria.lille.graphdep.util._
import org.rogach.scallop.ScallopConf
import scala.collection.SortedMap
import scala.xml.XML

/**
 * @author Romain Philippon
 */

case class Conf(arguments : Seq[String]) extends ScallopConf(arguments) {
  val path = opt[String]("path", short = 'p', descr = "Absolute path to the xml result file", required = true)
  val dates = opt[String]("dates", default = Some(""), noshort = true, descr = "Start and en date formatted as following dd/MM/yyyy")
  val add_dep_graph = opt[Boolean]("addcommit", short = 'a', default = Some(false))
  val del_dep_graph = opt[Boolean]("delcommit", short = 'd', default = Some(false))

  mutuallyExclusive(add_dep_graph, del_dep_graph)
}

object Main extends App {
  val parser = Conf(args)

  if(parser.add_dep_graph.apply) {
    createGraphNumberCommitNumberDependencyAddDep(parser.path.apply)
  }
  else if(parser.del_dep_graph.apply) {
    createGraphNumberCommitNumberDependencyDelDep(parser.path.apply)
  }
  else {
    if(parser.dates.apply.nonEmpty) {
      //val regex = "(([0-1][1-9]|3[0-2])/){2}[1-9][0-9]{3}".r

      //val res = for(date <- parser.dates.apply.size) yield regex.
      val dates = parser.dates.apply.split("-")

      createGraphDateNumberCommit(parser.path.apply, dates(0), dates(1))
    }
    else {
      createGraphDateNumberCommit(parser.path.apply)
    }
  }

  def isBetween(checkDate : DateTime, firstDate : DateTime, lastDate : DateTime) : Boolean = {
    if(checkDate.isAfter(firstDate) && checkDate.isBefore(lastDate)) {
      true
    }
    else {
      false
    }
  }

  def sameDate(date1 : DateTime, date2 : DateTime) : Boolean = {
    val simpleDate1 : LocalDate = new LocalDate(date1.getYear, date1.getMonthOfYear, date1.getDayOfMonth)
    val simpleDate2 : LocalDate = new LocalDate(date2.getYear, date2.getMonthOfYear, date2.getDayOfMonth)

    simpleDate1.equals(simpleDate2)
  }

  def createGraphDateNumberCommit(xmlPath : String, date1 : String, date2 : String) : Unit = {
    val formatter : DateTimeFormatter = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
    var commitDate, lastCommitDate : DateTime = null // contains the commit date of the current visited commit node while parsing
    val dateFirst = DateTimeFormat.forPattern("dd/MM/yyyy").parseDateTime(date1)
    val dateLast = DateTimeFormat.forPattern("dd/MM/yyyy").parseDateTime(date2)

    /* PARSING INITIALIZATION */
    lazy val xmlFile = XML.loadFile(xmlPath)
    lazy val commits = xmlFile \\ "analysis" \\ "history" \\ "commit"

    val graph : GraphTimeAmount = GraphTimeAmount("Graphe généré pour le dépôt : "+ (xmlFile \\ "analysis" \\ "git-repository").text, "Date (année)", "Nombre de commits")
    var nbAddedDep = 0
    var nbDeletedDep = 0
    lastCommitDate = formatter.parseDateTime((commits.head \\ "date").text)

    /* PARSE XML RESULT FILE TO COLLECT INFORMATION DAY BY DAY */
    commits.tail foreach { commit =>
      commitDate = formatter.parseDateTime((commit \\ "date").text)

      if(lastCommitDate == null) {
        lastCommitDate = commitDate
      }

      if(isBetween(commitDate, dateFirst, dateLast)) {
        if (sameDate(lastCommitDate, commitDate)) {
          /* UPDATE INFORMATION FOR THE DAY */
          nbAddedDep += (commit \\ "dependencies" \\ "added-dependency").size
          nbDeletedDep += (commit \\ "dependencies" \\ "deleted-dependency").size * -1
        }
        else {
          /* ADD NEW COMMIT DATE INFORMATION */
          if (nbAddedDep > 0) {
            graph.add(ValueDateAmount(lastCommitDate, nbAddedDep))
          }

          if (nbDeletedDep < 0) {
            graph.add(ValueDateAmount(lastCommitDate, nbDeletedDep))
          }

          nbAddedDep = (commit \\ "dependencies" \\ "added-dependency").size
          nbDeletedDep = (commit \\ "dependencies" \\ "deleted-dependency").size * -1
          lastCommitDate = commitDate
        }
      }
    }

    /* GENERATE SVG FILE */
    export(drawGraph(Dimension(1024, 756), graph))
  }

  def createGraphDateNumberCommit(xmlPath : String): Unit = {
    val formatter : DateTimeFormatter = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss");
    var commitDate, lastCommitDate : DateTime = null // contains the commit date of the current visited commit node while parsing

    /* PARSING INITIALIZATION */
    lazy val xmlFile = XML.loadFile(xmlPath)
    lazy val commits = xmlFile \\ "analysis" \\ "history" \\ "commit"

    val graph : GraphTimeAmount = GraphTimeAmount("Graphe généré pour le dépôt : "+ (xmlFile \\ "analysis" \\ "git-repository").text, "Date (année)", "Nombre de commits")
    var nbAddedDep = (commits.head \\ "dependencies" \\ "added-dependency").size
    var nbDeletedDep = (commits.head \\ "dependencies" \\ "deleted-dependency").size
    lastCommitDate = formatter.parseDateTime((commits.head \\ "date").text)

    /* PARSE XML RESULT FILE TO COLLECT INFORMATION DAY BY DAY */
    commits.tail foreach { commit =>
      commitDate = formatter.parseDateTime((commit \\ "date").text)

      if (sameDate(lastCommitDate, commitDate)) {
        /* UPDATE INFORMATION FOR THE DAY */
        nbAddedDep += (commit \\ "dependencies" \\ "added-dependency").size
        nbDeletedDep += (commit \\ "dependencies" \\ "deleted-dependency").size * -1
      }
      else {
        /* ADD NEW COMMIT DATE INFORMATION */
        if(nbAddedDep > 0) {
          graph.add(ValueDateAmount(lastCommitDate, nbAddedDep))
        }

        if(nbDeletedDep < 0) {
          graph.add(ValueDateAmount(lastCommitDate, nbDeletedDep))
        }

        nbAddedDep = (commit \\ "dependencies" \\ "added-dependency").size
        nbDeletedDep = (commit \\ "dependencies" \\ "deleted-dependency").size * -1
        lastCommitDate = commitDate
      }
    }

    /* GENERATE SVG FILE */
    export(drawGraph(Dimension(1024, 756), graph))
  }

  def createGraphNumberCommitNumberDependencyAddDep(xmlPath : String) : Unit = {
    /* PARSING INITIALIZATION */
    lazy val xmlFile = XML.loadFile(xmlPath)
    lazy val commits = xmlFile \\ "analysis" \\ "history" \\ "commit"
    var map : Map[Int, Int] = Map()
    val graph : GraphCommitDependency = GraphCommitDependency("Graphe généré pour le dépôt : "+ (xmlFile \\ "analysis" \\ "git-repository").text, "Nombre de dépendences ajoutées", "Nombre de commits")

    commits foreach { commit =>
      val nbAddedDep = (commit \\ "dependencies" \\ "added-dependency").size

      map.get(nbAddedDep) match {
        case Some(amount) => map = map.updated(nbAddedDep, amount + 1)
        case None => map += nbAddedDep -> 1
      }
    }

    val sorted = SortedMap.empty[Int, Int] ++ map

    println(sorted)

    sorted foreach { case(nbDependency : Int, amount : Int) =>
      graph.add(ValueCommitDependency(nbDependency, amount))
    }

    export(drawGraph(Dimension(1024, 756), graph))
  }

  def createGraphNumberCommitNumberDependencyDelDep(xmlPath : String) : Unit = {
    /* PARSING INITIALIZATION */
    lazy val xmlFile = XML.loadFile(xmlPath)
    lazy val commits = xmlFile \\ "analysis" \\ "history" \\ "commit"
    var map : Map[Int, Int] = Map()
    val graph : GraphCommitDependency = GraphCommitDependency("Graphe généré pour le dépôt : "+ (xmlFile \\ "analysis" \\ "git-repository").text, "Nombre de dépendences supprimées", "Nombre de commits")

    commits foreach { commit =>
      val nbDelDep = (commit \\ "dependencies" \\ "deleted-dependency").size

      map.get(nbDelDep) match {
        case Some(amount) => map = map.updated(nbDelDep, amount + 1)
        case None => map += nbDelDep -> 1
      }
    }

    val sorted = SortedMap.empty[Int, Int] ++ map

    sorted foreach { case(nbDependency : Int, amount : Int) =>
      graph.add(ValueCommitDependency(nbDependency, amount))
    }

    export(drawGraph(Dimension(1024, 756), graph))
  }
}
