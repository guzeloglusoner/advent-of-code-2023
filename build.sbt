ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2023",
    idePackagePrefix := Some("org.guzeloglu.application")
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0"

