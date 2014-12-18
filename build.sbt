name := "GraphDep"

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-unchecked", "-deprecation")

mainClass in Compile := Some("fr.inria.lille.graphdep.Main")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"