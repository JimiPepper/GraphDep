name := "GraphDep"

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-unchecked", "-deprecation")

mainClass in Compile := Some("fr.inria.lille.graphdep.Main")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "joda-time" % "joda-time" % "2.6",
  "org.rogach" %% "scallop" % "0.9.5"
)