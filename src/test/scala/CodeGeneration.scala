package com.github.jedesah

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBoxError, ToolBox}

import org.specs2.mutable._
import com.github.jedesah.IdiomBracket.ContextSubset
import utils._

class CodeGeneration extends Specification {

  class DefaultContext extends ContextSubset[reflect.runtime.universe.type] {
    var number = 0
    def freshName() = {
      number += 1
      "x" + number
    }
    def abort(pos: reflect.runtime.universe.Position, msg: String) = { throw new MacroAborted(msg) }
    def enclosingPosition= reflect.runtime.universe.NoPosition
  }

  case class MacroAborted(msg: String) extends Exception(msg)

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
    transformImpl(block, Last(nbLines), monadic)
  }

  def transformImpl(block: reflect.runtime.universe.Tree, range: Range, monadic: Boolean = false) = {
    val extractImport = q"import com.github.jedesah.IdiomBracket.extract"
    val tb = cm.mkToolBox()
    val everythingTyped = tb.typecheck(Block(extractImport :: block.children.init, block.children.last))
    val lastLines = everythingTyped.children.drop(1).slice(range).toList
    val testAST = if(lastLines.size == 1)lastLines.head else Block(lastLines.init, lastLines.last)
    tb.untypecheck(IdiomBracket.transform(scala.reflect.runtime.universe)(new DefaultContext,testAST, q"App", everythingTyped.tpe, monadic).get)
  }

  def transform(block: reflect.runtime.universe.Tree, ignore: Int, monadic: Boolean = false) = {
   transformImpl(block, Drop(ignore), monadic)
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
                    App.map(a)(x1 => {val aa = otherThing(x1); otherThing(aa)})
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
    "with currying" in {
      val ast = q"""
                def test(a: String)(b: String): String  = ???
                val a: Option[String] = ???
                test(extract(a))("foo")
              """
      val transformed = transformLast(ast)
      val expected = q"""
                       App.map(a)(x1 => test(x1)("foo"))
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
    }.pendingUntilFixed("not sure how to pass in the type that is an Applicative directly to the genreation function")
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
        loc match {
          case loc: City => App.map(name)(x3 => loc.copy(city = Some(x3)))
          case loc: Country => App.map(name)(x4 => loc.copy(countryName = Some(x4)))
          case _ => App.pure(loc)
        }
      """
      compareAndPrintIfDifferent(transformed, expected)
    }.pendingUntilFixed("This is pretty close, and works in the actual Spec for now")
    "canBuildFrom" in {
      val ast = q"""
        val a: Option[List[String]] = ???
        def b: String => String = ???
        extract(a).map(b)
      """
      val transformed = transform(ast, ignore = 2)
      val expected = q"""
        App.map(a)(x1 => x1.map[String, List[String]](b)(immutable.this.List.canBuildFrom[String]))
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
        App.map(a)(x1 => test(x1)(myProof))
      """
      compareAndPrintIfDifferent(transformed, expected)
    }
    "SIP-22 example" in {
      val ast = q"""
        val optionDOY: Option[String] = ???
        val date = "(\\d+)/(\\d+)".r
        case class Ok(message: String)
        case class NotFound(message: String)
        def nameOfMonth(num: Int): Option[String] = None
        extract (optionDOY) match {
          case date(month, day) =>
            Ok(s"It’s $${extract(nameOfMonth(month.toInt))}!")
          case _ =>
            NotFound("Not a date, mate!")
        }
      """
      val transformed = transform(ast, ignore = 7, monadic = true)
      val expected = q"""
        App.bind(optionDOY){ doy =>
          doy match {
            case date(month, day) =>
              App.map(nameOfMonth(month.toInt))(x1 => Ok(s"It's $$x1!"))
            case _ =>
              NotFound("Not a date, mate!")
          }
      }
      """
      compareAndPrintIfDifferent(transformed, expected)
    }.pendingUntilFixed("Close Enough")
    "clean compilation error if monad is required" in {
      val ast = q"""
        val optionDOY: Option[String] = ???
        val date = "(\\d+)/(\\d+)".r
        case class Ok(message: String)
        case class NotFound(message: String)
        def nameOfMonth(num: Int): Option[String] = None
        extract (optionDOY) match {
          case date(month, day) =>
            Ok(s"It’s $${extract(nameOfMonth(month.toInt))}!")
          case _ =>
            NotFound("Not a date, mate!")
        }
      """
      transform(ast, ignore = 7) must throwA[MacroAborted](message = "This expression requires an instance of Monad")
    }
    "so many parameters" in {
      val ast = q"""
        val fun: (String, String, String, String, String, String, String, String, String, String, String, String, String) => String = ???
        val a: Option[String] = ???
        val b: Option[String] = ???
        val c: Option[String] = ???
        val d: Option[String] = ???
        val e: Option[String] = ???
        val f: Option[String] = ???
        val g: Option[String] = ???
        val h: Option[String] = ???
        val i: Option[String] = ???
        val j: Option[String] = ???
        val k: Option[String] = ???
        val l: Option[String] = ???
        val m: Option[String] = ???
        fun(
          extract(a),
          extract(b),
          extract(c),
          extract(d),
          extract(e),
          extract(f),
          extract(g),
          extract(h),
          extract(i),
          extract(j),
          extract(k),
          extract(l),
          extract(m))
      """
      val expected = q"""
        App.map(sequence(x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: HNil)) {
          case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: HNil =>
            fun(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
        }
      """
      compareAndPrintIfDifferent(transformLast(ast), expected)
    }.pendingUntilFixed("Don't have time to figure out HList problem right now")
  }
}