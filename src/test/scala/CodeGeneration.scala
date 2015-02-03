package com.github.jedesah

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError

import org.specs2.mutable._
import com.github.jedesah.IdiomBracket.ContextSubset

class CodeGeneration extends Specification {

  class DefaultContext extends ContextSubset[reflect.runtime.universe.type] {
    var number = 0
    def freshName() = {
      number += 1
      "x" + number
    }
    def abort(pos: reflect.runtime.universe.Position, msg: String) = { throw new IllegalArgumentException("macro test was aborted") }
    def enclosingPosition= reflect.runtime.universe.NoPosition
  }

  def compareAndPrintIfDifferent(actual: reflect.runtime.universe.Tree, expected: reflect.runtime.universe.Tree, compareString: Boolean = false) = {
    val areEqual = if(compareString) actual.toString == expected.toString else actual equalsStructure expected
    if (areEqual) true
    else {
      println("ACTUAL PRETTY:\n" + actual)
      println("EXPECTED PRETTY:\n" + expected)
      println("ACTUAL RAW:\n" + showRaw(actual))
      println("EXPECTED RAW:\n" + showRaw(expected))
      false
    }
  }

  def transformLast(block: reflect.runtime.universe.Tree, nbLines: Int = 1, monadic: Boolean = false) = {
    val extractImport = q"import com.github.jedesah.IdiomBracket.auto.extract"
    val tb = cm.mkToolBox()
    val lastLines = tb.typecheck(Block(extractImport :: block.children.init, block.children.last)).children.takeRight(nbLines)
    val testAST = if(nbLines == 1)lastLines.head else Block(lastLines.init, lastLines.last)
    tb.untypecheck(IdiomBracket.transform(scala.reflect.runtime.universe)(new DefaultContext,testAST, q"App", monadic).get)
  }

  "code generation" should {
    "simple function invocation" in {
      val ast = q"""
                  def doThing(a: String, b: String) = ???
                  val a: Option[String] = ???
                  val b: Option[String] = ???
                  doThing(extract(a), extract(b))
                """
      val transformed = transformLast(ast)
      val expected = q"""
                      App.apply2(a,b)(doThing)
                     """
      compareAndPrintIfDifferent(transformed, expected)
    }
    "complex method invocation" in {
      //IdiomBracket(doThing(extract(a), extract(c).toString).indexOf(b, extract(c)))
      val ast = q"""
                def doThing(a: String, b: String): String = ???
                val a: Option[String] = ???
                val b: Int = ???
                val c: Option[Int] = ???
                doThing(extract(a), extract(c).toString).indexOf(b, extract(c))
              """
      val transformed = transformLast(ast)
      val expected = q"""
                    App.apply3(
                      App.apply2(
                        a,
                        App.map(c)(x4 => x4.toString())
                      )(doThing),
                      App.pure(b),
                      c
                    )((x1, x2, x3) => x1.indexOf(x2,x3))
                   """
      compareAndPrintIfDifferent(transformed, expected, compareString = true)
    }
    "recursive" in {
      val ast = q"""
                def doThing(a: String, b: String, c: String): String = ???
                def otherThing(a: String): String = ???
                val a,b,c: Option[String] = ???
                doThing(otherThing(extract(a)),extract(b), extract(c))
              """
      val transformed = transformLast(ast)
      val expected = q"""
                    App.apply3(App.map(a)(otherThing),b,c)(doThing)
                  """
      compareAndPrintIfDifferent(transformed, expected)
    }
    "with block" in {
      val ast = q"""
                def otherThing(a: String): String = a
                val a: Option[String] = Option("hello")
                val b: Option[String] = Some("h")
                val aa = otherThing(extract(a))
                otherThing(aa)
              """
      val transformed = transformLast(ast, nbLines = 2)
      val expected = q"""
                    {val aa = App.map(a)(otherThing); App.map(aa)(otherThing)}
                   """
      compareAndPrintIfDifferent(transformed, expected)
    }
    tag("match")
    "of match with extract in LHS" in {
      "1" in {
        val ast = q"""
                  val a: Option[String] = ???
                  extract(a) match { case "hello" => "h" }
                """
        val transformed = transformLast(ast)
        val expected = q"""
                      App.map(a)(((x1) => x1 match {
                        case "hello" => "h"
                      }))
                     """
        compareAndPrintIfDifferent(transformed, expected, compareString = false)
      }
      "2" in {
        val ast = q"""
                val a: Option[String] = ???
                extract(a) match {
                  case "hello" => "h"
                  case _ => "e"
                }
              """
        val transformed = transformLast(ast)
        val expected = q"""
                    App.map(a)(((x1) => x1 match {
                      case "hello" => "h"
                      case _ => "e"
                    }))
                   """
        compareAndPrintIfDifferent(transformed, expected, compareString = false)
      }
      "with stable identifier referring to extracted val" in {
        val ast = q"""
                val a: Option[String] = ???
                val b: Option[String] = ???
                val bb = extract(b)
                extract(a) match {
                  case `bb` => "h"
                  case _ => "e"
                }
              """
        val transformed = transformLast(ast, nbLines = 2)
        val expected = q"""
                    val bb = b
                    App.apply2(a,bb)((x2,x1) => x2 match {
                      case `x1` => "h"
                      case _ => "e"
                    })
                   """
        compareAndPrintIfDifferent(transformed, expected)
      }
    }
    tag("interpolated string")
    "interpolated string" in {
      val ast = q"""
                val a: Option[String] = ???
                s"It's $${extract(a)}!"
              """
      val transformed = transformLast(ast)
      val expected = q"""
                       App.map(a)(((x1) => scala.StringContext.apply("It\'s ", "!").s(x1)))
                    """
      compareAndPrintIfDifferent(transformed, expected, compareString = true)
    }
  }
}