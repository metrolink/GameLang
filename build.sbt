ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.9"

lazy val root = (project in file("."))

  .settings(
    name := "PrisonRework"
  )


val AkkaVersion = "2.7.0"
libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.4"