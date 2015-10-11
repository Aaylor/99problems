
name := "scala-99problems"

scalaVersion := "2.11.5"

scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature"
  )

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
