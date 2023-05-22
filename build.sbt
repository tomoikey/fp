ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"

lazy val root = (project in file(".")).settings(name := "fp")
