name := "idiom-bracket"

organization := "com.github.jedesah"

startYear := Some(2014)

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.2",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.specs2" %% "specs2-core" % "3.5" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.5" % "test",
  "com.chuusai" %% "shapeless" % "2.1.0",
  "org.typelevel" %% "shapeless-scalaz" % "0.3",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"
)

initialCommands += "import scala.reflect.runtime.universe._"
    