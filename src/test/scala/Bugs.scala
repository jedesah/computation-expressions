package com.github.jedesah

import org.specs2.ScalaCheck
import org.specs2.mutable._
import IdiomBracket.extract

import scalaz.std.option._

import scalaz.Apply

class Bugs extends Specification with ScalaCheck {

  "bugs" should {
    "canBuildFrom" ! prop { (a: Option[List[String]], b: String => String) =>
      val f = IdiomBracket[Option, List[String]](extract(a).map(b))
      f ==== Apply[Option].map(a)(_.map(b))
    }
  }
}