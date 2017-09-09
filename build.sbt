import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "me.bowdon",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ddl-diff",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
    )
  )
