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
    val extractImport = q"import com.github.jedesah.IdiomBracket.extract"
    val tb = cm.mkToolBox()
    val everythingTyped = tb.typecheck(Block(extractImport :: block.children.init, block.children.last))
    val lastLines = everythingTyped.children.takeRight(nbLines)
    val testAST = if(lastLines.size == 1)lastLines.head else Block(lastLines.init, lastLines.last)
    tb.untypecheck(IdiomBracket.transform(scala.reflect.runtime.universe)(new DefaultContext,testAST, q"App", everythingTyped.tpe, monadic).get)
  }

  def transform(block: reflect.runtime.universe.Tree, ignore: Int, monadic: Boolean = false) = {
    val extractImport = q"import com.github.jedesah.IdiomBracket.extract"
    val tb = cm.mkToolBox()
    val everythingTyped = tb.typecheck(Block(extractImport :: block.children.init, block.children.last))
    val lastLines = everythingTyped.children.drop(ignore + 1)
    val testAST = if(lastLines.size == 1)lastLines.head else Block(lastLines.init, lastLines.last)
    val transformed = IdiomBracket.transform(scala.reflect.runtime.universe)(new DefaultContext,testAST, q"App", everythingTyped.tpe, monadic).get
    println(transformed)
    tb.untypecheck(transformed)
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
                        App.map(c)(x3 => x3.toString())
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
    "of match" in {
      "extract in RHS" in {
        val ast = q"""
                     val a: Option[String] = ???
                     List(1,2,3) match {
                        case Nil => extract(a) + "!"
                        case _ => "hello"
                     }
                  """
        val transformed = transformLast(ast)
        val expected = q"""
                           App.map(a)(x1 => immutable.this.List.apply[Int](1,2,3) match {
                              case immutable.this.Nil => x1.+("!")
                              case _ => "hello"
                            })
                  """
        compareAndPrintIfDifferent(transformed, expected)
      }.pendingUntilFixed("Fails because Scala sucks at comparing ASTs")
      "with extract in LHS" in {
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
                       App.map(a)(x2 => test(x2)("foo"))
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
    "val definition pattern matching" in {
      val ast = q"""
                   val a: Option[String] = ???
                   def test(a: String): (String, String) = ???
                   val (first, second) = test(extract(a))
                   first + second
                """
      val transformed = transform(ast, monadic = false, ignore = 2)
      val expected = q"""
                      App.map(a){ x1 =>
                        val (first, second) = test(x1)
                        first + second
                      }
                      """
      compareAndPrintIfDifferent(transformed, expected)
    }.pendingUntilFixed("It's quite hard to support pattern matching value definitions because of the complex desaguring involved (which happens before the Tree is passed into the macro)")
    "monadic block" in {
      val ast = q"""
                   val a: Option[String] = ???
                   def foo(a: String): Option[String] = ???
                   def bar(a: String): String = ???
                   val b = foo(extract(a))
                   bar(extract(b))
                """
      val transformed = transformLast(ast, monadic = true, nbLines = 2)
      val expected = q"""
                      val b = App.map(a)(foo)
                      App.map(App.join(b))(bar)
                      """
      compareAndPrintIfDifferent(transformed, expected)
    }
    "iGraph options stuff" in {
      val ast = q"""
        trait LocationType
        case class City(city: Option[String]) extends LocationType
        case class Country(countryName: Option[String]) extends LocationType
        case object State extends LocationType
        val loc: LocationType = City(None)
        // 'EN-US' will be "normalized" to 'en'
        val userLocale: String = ???
        def normalize(locale: String) = locale.take(2).toLowerCase
        val namesOption: Option[Map[String, String]] = ???
        val names = extract(namesOption)
        val normalizedMap = names.map { case (key, value) => (normalize(key), value)}
        val name = extract(normalizedMap.get(normalize(userLocale)).orElse(normalizedMap.get("en")))
        // If there is no user requested locale or english, leave Location unchanged
        loc match {
          case loc:City => loc.copy(city = Some(name))
          case loc:Country => loc.copy(countryName = Some(name))
          case _ => loc
        }
      """
      val transformed = transform(ast, monadic = true, ignore = 10)
      val expected = q"""
        val names = namesOption
        val normalizedMap = App.map(names)(x1 => x1.map { case (key, value) => (normalize(key), value)})
        val name = App.bind(normalizedMap)(x2 => x2.get(normalize(userLocale)).orElse(normalizeMap.get("en")))
        App.map(name)(x3 => loc match {
          case loc: City => loc.copy(city = Some(x3))
          case loc: Country => loc.copy(countryName = Some(x3))
          case _ => loc
        })
      """
      compareAndPrintIfDifferent(transformed, expected)
    }
    "canBuildFrom" in {
      val ast = q"""
        val a: Option[List[String]] = ???
        def b: String => String = ???
        extract(a).map(b)
      """
      val transformed = transform(ast, ignore = 2)
      val expected = q"""
        App.map(a)(x2 => x2.map(b)(immutable.this.List.canBuildFrom[String]))
      """
      compareAndPrintIfDifferent(transformed, expected, compareString = true)
    }
    "implicit currying" in {
      val ast = q"""
        val a: Option[String] = ???
        val b: String => String = ???
        val c: String = ???
        trait Proof
        def test(fst: String)(implicit snd: Proof): String = b(fst)
        implicit val myProof = new Proof {}
        test(extract(a))
      """
      val transformed = transform(ast, ignore = 6)
      val expected = q"""
        App.map(a)(x2 => test(x2)(myProof))
      """
      compareAndPrintIfDifferent(transformed, expected)
    }
  }
}