package com.github.jedesah

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

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
    // There must be a better way of doing this!
    if (areEqual) true ==== true
    else {
      //println("ACTUAL RAW:\n" + showRaw(actual))
      //println("EXPECTED RAW:\n" + showRaw(expected))
      actual ==== expected
    }
  }

  def transformLast(block: reflect.runtime.universe.Tree, nbLines: Int = 1, monadic: Boolean = false) = {
    val extractImport = q"import com.github.jedesah.IdiomBracket.auto.extract"
    val tb = cm.mkToolBox()
    val everythingTyped = tb.typecheck(Block(extractImport :: block.children.init, block.children.last))
    val lastLines = everythingTyped.children.takeRight(nbLines)
    val testAST = if(nbLines == 1)lastLines.head else Block(lastLines.init, lastLines.last)
    tb.untypecheck(IdiomBracket.transform(scala.reflect.runtime.universe)(new DefaultContext,testAST, q"App", everythingTyped.tpe, monadic).get)
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
                    App.apply2(
                      App.apply2(
                        a,
                        App.map(c)(x5 => x5.toString())
                      )(doThing),
                      c
                    )((x1, x2) => x1.indexOf(b,x2))
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
    "if statement" in {
      val ast = q"""
                  val a: Option[String] = ???
                  if (extract(a).length == 5) 10 else 20
                """
      val transformed = transformLast(ast)
      val expected = q"""
                      App.map(a)(x1 => if(x1.length().==(5)) 10 else 20)
                     """
      compareAndPrintIfDifferent(transformed, expected)
    }.pendingUntilFixed("This test does not work because the detection of one extract does not work and I am too lazy to come up with the current code generation")
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
    "with currying" in {
      val ast = q"""
                def test(a: String)(b: String): String  = ???
                val a: Option[String] = ???
                test(extract(a))("foo")
              """
      val transformed = transformLast(ast)
      val expected = q"""
                       App.map(App.map(a)(test))((x1) => x1.apply("foo"))
                    """
      compareAndPrintIfDifferent(transformed, expected, compareString = true)
    }
    "with typed" in {
      val ast = q"""
                def test(a: String): String  = ???
                val a: Option[String] = ???
                test(extract(a)): String
              """
      val transformed = transformLast(ast)
      val expected = q"""
                       App.map(a)(test): Option[String]
                    """
      compareAndPrintIfDifferent(transformed, expected, compareString = true)
    }.pendingUntilFixed("not sure how to pass in the the type that is an Applicative directly to the genreation function")
    "asc reverse core site" in {
      val ast = q"""
                   val phone: Option[String] = ???
                   val hitCounter: Option[String] = ???
                   val locById: Option[String] = ???
                   def test(a: String, b: String): Option[(String, String)] = ???
                   def otherTest(a: String, b: String, c: String): Option[String] = ???
                   val tuple: (String, String) = extract(test(extract(phone), extract(hitCounter)))
                   extract(otherTest(tuple._2, tuple._1, extract(locById)))
                """
      val transformed = transformLast(ast, monadic = true, nbLines = 2)
      val expected = q"""
                      val tuple = App.bind2(phone, hitCounter)(test)
                      App.bind3(App.map(tuple)(_._1), App.map(tuple)(_._2), locById)(otherTest)
                      """
      compareAndPrintIfDifferent(transformed, expected)
    }.pendingUntilFixed("Don't know how to make this a deterministic test")
  }
}