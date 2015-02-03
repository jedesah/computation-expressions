package com.github.jedesah

import org.specs2.mutable._

import scala.tools.reflect.ToolBoxError
import scalaz.{Monad, Applicative}

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError

class NonCompilation extends Specification {

  "compile errors" should {
    "extract does not compile on it's own" in {
      val ast = q"""
                import com.github.jedesah.IdiomBracket.extract
                val a: Option[String] = ???
                def doThing(a: String): String = ???
                doThing(extract(a))
              """
      val tb = cm.mkToolBox()
      try {
        tb.compile(ast); 1 ==== 2 // There must be a better way...
      } catch {
        case a: ToolBoxError => a.getMessage.contains("`extract` must be enclosed in an `IdiomBracket`") ==== true
      }
    }
  }
}