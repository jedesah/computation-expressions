name := "idiom-bracket"

organization := "com.github.jedesah"

startYear := Some(2014)

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.2-SNAPSHOT",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.specs2" %% "specs2-core" % "2.4.15" % "test",
  "org.specs2" %% "specs2-scalacheck" % "2.4.15" % "test",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"
)

initialCommands += "import scala.reflect.runtime.universe._"
    