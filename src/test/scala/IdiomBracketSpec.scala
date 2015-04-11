package com.github.jedesah

import java.util.concurrent.TimeUnit

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.specs2.ScalaCheck
import org.specs2.mutable._
import IdiomBracket.extract

import scala.concurrent.Await
import scalaz.{Monad, Applicative, Apply}
import scalaz.std.option._
import scalaz.std.list._

class IdiomBracketSpec extends Specification with ScalaCheck {

  implicit def FutureArbitrary[A: Arbitrary]: Arbitrary[scala.concurrent.Future[A]] =
    Arbitrary(arbitrary[A] map ((x: A) => scala.concurrent.Future.successful(x)))

  "IdiomBracket" should {
    "simple function application" in {
      "2 params" ! prop { (a: Option[String], b: Option[String], doThing: (String, String) => String) =>
        val f = IdiomBracket[Option, String](doThing(extract[Option, String](a), extract[Option, String](b)))
        f ==== Apply[Option].apply2(a, b)(doThing)
      }
      "3 params" in {
        "all extracts" ! prop { (a: Option[String], b: Option[String], c: Option[String], doThing: (String, String, String) => String) =>
          val f = IdiomBracket[Option, String](doThing(extract(a), extract(b), extract(c)))
          f ==== Applicative[Option].apply3(a, b, c)(doThing)
        }
        "some extracts, some not " ! prop { (a: String, b: Option[String], c: Option[String], doThing: (String, String, String) => String) =>
          val f = IdiomBracket[Option, String](doThing(a, extract(b), extract(c)))
          f ==== Applicative[Option].apply3(Some(a),b,c)(doThing)
        }
      }
      tag("type parameter")
      "function with type parameters" ! prop { (a: Option[String], fooImpl: String => String, b: String) =>
        def foo[A](a: A, b: String): String = fooImpl(b) + a
        val f = IdiomBracket[Option, String](foo(b, extract(a)))
        f ==== Applicative[Option].map(a)(foo(b, _))
      }
    }
    "method invocation" in {
      "no extract in LHS" ! prop { (a: String, b: Option[Int], c: Option[Int]) =>
        val f = IdiomBracket[Option, Int](a.indexOf(extract(b), extract(c)))
        f ==== Applicative[Option].apply2(b, c)(a.indexOf(_, _))
      }
      "extract in LHS" ! prop { (a: Option[String], b: Int, c: Option[Int]) =>
        val f = IdiomBracket[Option, Int](extract(a).indexOf(b, extract(c)))
        f ==== Applicative[Option].apply2(a,c)(_.indexOf(b, _))
      }
      "complex method invocation" in {
        "1" ! prop { (a: Option[String], b: Int, c: Option[Int], doThing: (String, String) => String) =>
          val f = IdiomBracket[Option, Int](doThing(extract(a), extract(c).toString).indexOf(b, extract(c)))
          f ==== Applicative[Option].apply2(a, c)((aa, cc) => doThing(aa, cc.toString).indexOf(b, cc))
        }
        "2" ! prop { (a: Option[String], b: Int, c: Option[Int], d: Option[String], doThing: (String, String) => String) =>
          val f = IdiomBracket[Option, Int](doThing(extract(a), extract(d)).indexOf(b, extract(c)))
          f ==== Applicative[Option].apply3(a, c, d)((aa, cc, dd) => doThing(aa, dd).indexOf(b, cc))
        }
      }
      tag("type parameter")
      "method with type parameters" ! prop { (a: Option[Either[String, Int]]) =>
        val f = IdiomBracket[Option, Int](extract(a).fold(_.length,identity))
        f ==== Applicative[Option].map(a)(_.fold(_.length, identity))
      }
    }
    "extract buried" in {
      "deep" ! prop { (a: Option[String], b: Option[String], c: Option[String], doThing: (String, String, String) => String, otherThing: String => String) =>
        val f = IdiomBracket[Option, String](doThing(otherThing(extract(a)), extract(b), extract(c)))
        f ==== Applicative[Option].apply3(a, b, c)((aa, bb, cc) => doThing(otherThing(aa), bb, cc))
      }
      "deeper" ! prop { (a: Option[String], b: Option[String], c: Option[String], doThing: (String, String, String) => String, otherThing: String => String, firstThis: String => String) =>
        val f = IdiomBracket[Option, String](doThing(otherThing(firstThis(extract(a))), extract(b), extract(c)))
        f ==== Applicative[Option].apply3(a,b,c)((aa,bb,cc) => doThing(otherThing(firstThis(aa)), bb,cc))
      }
    }
    "monadic" in {
      "double nested extract within argument" in {
        "simple enough" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int], doThing: (String, String, String) => String, firstThis: String => Option[String]) =>
          val f = IdiomBracket.monad[Option, String](doThing(extract(firstThis(extract(a))), extract(b), extract(c)))
          f ==== Applicative[Option].apply3(Monad[Option].bind(a)(firstThis), b, c)(doThing)
        }
        "nested a little deeper" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int], doThing: (String, String, String) => String, firstThis: String => Option[String], other: String => String) =>
          val f = IdiomBracket.monad[Option, String](doThing(other(extract(firstThis(extract(a)))),extract(b), extract(c)))
          f ==== Applicative[Option].apply3(Applicative[Option].map(Monad[Option].bind(a)(firstThis))(other),b,c)(doThing)
        }
        "with 2 monads inside first extract" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int], doThing: (String, String) => String, firstThis: (String, String) => Option[String], other: String => String) =>
          val f = IdiomBracket.monad[Option, String](doThing(other(extract(firstThis(extract(a), extract(b)))), extract(c)))
          f == Applicative[Option].apply2(Applicative[Option].map(Monad[Option].bind2(a,b)(firstThis))(other),c)(doThing)
        }
        "tricky function that takes a monad and extracts itself. Want to make sure we are not to eager to lift things" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int], doThing: (String, String, String) => String, firstThis: String => String, other: String => String) =>
          val f = IdiomBracket.monad[Option, String](doThing(other(firstThis(extract(a))),extract(b), extract(c)))
          f ==== Applicative[Option].apply3(Applicative[Option].map(Applicative[Option].map(a)(firstThis))(other),b,c)(doThing)
        }
        "if/else expression that is like a monadic function" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int], doThing: (String, String, String) => String, other: String => String) =>
          val f = IdiomBracket.monad[Option, String]{doThing(other(extract(if (extract(a) == "") Some("a") else None)),extract(b), extract(c))}
          f ==== Applicative[Option].apply3(Applicative[Option].map(Monad[Option].bind(a)(f => if (f == "") Some("a") else None))(other),b,c)(doThing)
        }
        "interpolated String" ! prop {(a: Option[String]) =>
          val f = IdiomBracket.monad[Option, String] {s"It is ${extract(a)}!"}
          f ==== Applicative[Option].map(a)(aa => s"It is $aa!")
        }
      }
      "is lazy with if/else" in {
        import scala.concurrent.Future
        import scala.concurrent.Promise
        import scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.duration._

        val aPromise = Promise[Boolean]()
        val a = aPromise.future

        val bPromise = Promise[String]()
        val b = bPromise.future

        val cPromise = Promise[String]()
        val c = cPromise.future

        implicit val monad: Monad[Future] = scalaz.std.scalaFuture.futureInstance

        val f = IdiomBracket.monad[Future, String](if(extract(a)) extract(b) else extract(c))

        f.value ==== None

        aPromise.success(true)

        f.value ==== None

        bPromise.success("hello")

        Await.result(f, FiniteDuration(1, TimeUnit.SECONDS)) ==== "hello"
      }
      tag("block")
      "block" in {
        "1" ! prop { (a: Option[String], foo: String => Option[String], bar: String => String) =>
          val f = IdiomBracket.monad[Option, String] {
            val b = foo(extract(a))
            bar(extract(b))
          }
          f ==== Monad[Option].bind(a)(aa => Applicative[Option].map(foo(aa))(bar))
        }
        "2" ! prop { (a: Option[String], d: Option[String], foo: String => Option[String], bar: (String, String) => String, biz: String => Option[String]) =>
          val f = IdiomBracket.monad[Option, String] {
            val b = foo(extract(a))
            val c = biz(extract(d))
            bar(extract(b), extract(c))
          }
          f ==== {
            val b = Applicative[Option].map(a)(foo)
            val c = Applicative[Option].map(d)(biz)
            Applicative[Option].apply2(Monad[Option].join(b),Monad[Option].join(c))(bar)
          }
        }
      }
    }
    tag("block")
    "block" in {
      "simple" ! prop { (a: Option[String], otherThing: String => String) =>
        val f = IdiomBracket[Option, String] {
          otherThing(extract(a))
        }
        f ==== Applicative[Option].map(a)(otherThing)
      }
      "slighly more complex is a useless way you would think" ! prop { (a: Option[String], otherThing: String => String) =>
        val f = IdiomBracket[Option, String] {
          otherThing(extract(a))
          otherThing(extract(a))
        }
        f ==== Applicative[Option].map(a)(otherThing)
      }
      "pointless val" ! prop { (a: Option[String], otherThing: String => String) =>
        val f = IdiomBracket[Option, String] {
          val aa = otherThing(extract(a))
          aa
        }
        f ==== Applicative[Option].map(a)(otherThing)
      }
      "slightly less simple and somewhat useful" ! prop { (a: Option[String], otherThing: String => String) =>
        val f = IdiomBracket[Option, String] {
          val aa = otherThing(extract(a))
          otherThing(aa)
        }
        f ==== Applicative[Option].map(a)(aa => otherThing(otherThing(aa)))
      }
      /*"val pattern match" ! prop { (a: Option[String], test: String => (String, String)) =>
        val f = IdiomBracket[Option, String] {
          val (first, second) = test(extract(a))
          first + second
        }
        f ==== Applicative[Option].map(a){aa =>
          val (first, second) = test(aa)
          first + second
        }
      }*/
      tag("match")
      "with match (iGraph example)" ! prop { (namesOption: Option[Map[String, String]], userLocale: String) =>
        sealed trait LocationType
        case class City(city: Option[String]) extends LocationType
        case class Country(countryName: Option[String]) extends LocationType
        case object State extends LocationType
        val loc: LocationType = City(None)
        // 'EN-US' will be "normalized" to 'en'
        def normalize(locale: String) = locale.take(2).toLowerCase
        val f = IdiomBracket.monad[Option, LocationType] {
          val names = extract(namesOption)
          val normalizedMap = names.map { case (key, value) => (normalize(key), value)}
          val name = extract(normalizedMap.get(normalize(userLocale)).orElse(normalizedMap.get("en")))
          // If there is no user requested locale or english, leave Location unchanged
          loc match {
            case loc:City => loc.copy(city = Some(name))
            case loc:Country => loc.copy(countryName = Some(name))
            case _ => loc
          }
        }
        val expected = {
          val names = namesOption
          val normalizedMap = Applicative[Option].map(names)(x1 => x1.map { case (key, value) => (normalize(key), value)})
          val name = Monad[Option].bind(normalizedMap)(x1 => x1.get(normalize(userLocale)).orElse(x1.get("en")))
          loc match {
            case loc: City => Applicative[Option].map(name)(x1 => loc.copy(city = Some(x1)))
            case loc: Country => Applicative[Option].map(name)(x1 => loc.copy(countryName = Some(x1)))
            case _ => Applicative[Option].pure(loc)
          }
        }
        f ==== expected
      }
    }
    tag("match")
    "match" in {
      "with extract in expression" ! prop { (a: Option[String]) =>
        val f = IdiomBracket[Option, String] {
          extract(a) match {
            case "hello" => "h"
            case _ => "e"
          }
        }
        if (a.isDefined)
          f == Some(a.get match {
            case "hello" => "h"
            case _ => "e"
          })
        else
          f == None
      }
      "with extract in RHS of case statement" ! prop { a: Option[String] =>
        val f = IdiomBracket[Option, String] {
          List(1,2,3) match {
            case Nil => extract(a) + "!"
            case _ => "hello"
          }
        }
        val expected = List(1,2,3) match {
          case Nil => Applicative[Option].map(a)(_ + "!")
          case _ => Applicative[Option].pure("hello")
        }
        f ==== expected
      }
      "with stable identifier in  pattern match case statement" ! prop { (a: Option[String], b: Option[String]) =>
        val f = IdiomBracket[Option, String] {
          val bb = extract(b)
          extract(a) match {
            case `bb` => "h"
            case _ => "e"
          }
        }
        val expected = Applicative[Option].apply2(a,b)((a,b) =>
          a match {
            case `b` => "h"
            case _ => "e"
          }
        )
        f == expected
      }
      "monadic" in {
        "simple" ! prop { (a: Option[String], b: Option[String], c: Option[String], foo: String => String, bar: String => String) =>
          val f = IdiomBracket.monad[Option, String] {
            extract(a) match {
              case "" => foo(extract(b))
              case _ => bar(extract(c))
            }
          }
          val expected = Monad[Option].bind(a){
            case "" => Apply[Option].map(b)(foo)
            case _ => Apply[Option].map(c)(bar)
          }
          f ==== expected
        }
        "more complex" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[String], foo: String => String, bar: String => String) =>
          val f = IdiomBracket.monad[Option, String] {
            val dd = extract(d)
            extract(a) match {
              case `dd` => foo(extract(b))
              case _ => bar(extract(c))
            }
          }
          val expected = Monad[Option].bind2(a, d) { (aa, dd) => aa match {
            case `dd` => Apply[Option].map(b)(foo)
            case _ => Apply[Option].map(c)(bar)
          }
          }
          f ==== expected
        }
        "with multiple stable identifier in the pattern matches of case statements" ! prop {
          (a: Option[String],
           b: Option[String],
           c: Option[String],
           d: Option[String],
           e: Option[String],
           f: String,
           foo: String => String,
           bar: String => String) =>
          val result = IdiomBracket.monad[Option, String] {
            val dd = extract(d)
            val ee = extract(e)
            extract(a) match {
              case `dd` => foo(extract(b))
              case `ee` => bar(extract(c))
              case _ => f
            }
          }
          val expected = Monad[Option].bind2(a, d) { (aa, dd) => aa match {
            case `dd` => Apply[Option].map(b)(foo)
            case _ => Apply[Option].map(c)(bar)
          }
          }
          result ==== expected
        }.pendingUntilFixed("Not yet implemented")
      }
    }
    "if statement" in {
      "extract in condition expression" ! prop { (a: Option[String]) =>
        val f = IdiomBracket[Option, Int] {
          if (extract(a).length == 5) 10 else 20
        }
        f ==== Applicative[Option].map(a)(aa => if (aa.length == 5) 10 else 20)
      }
    }
    tag("funky")
    "renamed import" ! prop { (a: Option[String], b: Option[String], doThing: (String, String) => String) =>
      import IdiomBracket.{extract => extractt}
      val f = IdiomBracket[Option, String](doThing(extractt(a),extractt(b)))
      f == Applicative[Option].apply2(a,b)(doThing)
    }
    tag("funky")
    "implicit extract" ! prop { (a: Option[String], b: Option[String], doThing: (String, String) => String) =>
      import IdiomBracket.auto.extract
      val f = IdiomBracket[Option, String](doThing(a,b))
      f == Applicative[Option].apply2(a,b)(doThing)
    }
    /*"SIP-22 example" ! prop { (optionDOY: Option[String]) =>
      val date = """(\d+)/(\d+)""".r
      case class Ok(message: String)
      case class NotFound(message: String)
      def nameOfMonth(num: Int): Option[String] = None

      val f = IdiomBracket.monad[Option, Any] {
        extract(optionDOY) match {
          case date(month, day) =>
            Ok(s"It’s ${extract(nameOfMonth(month.toInt))}!")
          case _ =>
            NotFound("Not a date, mate!")
        }
      }
    }*/
    "asc reverse core site" in {
      "without val pattern match" ! prop { (phone: Option[String], hitCounter: Option[String], locById: Option[String]) =>
        def test(a: String, b: String): Option[(String, String)] = Some((a, b))
        def otherTest(a: String, b: String, c: String): Option[String] = Some(a)

        val result = IdiomBracket.monad[Option, String] {
          val tuple: (String, String) = extract(test(extract(phone), extract(hitCounter)))
          extract(otherTest(tuple._2, tuple._1, extract(locById)))
        }
        val first = Monad[Option].bind2(phone, hitCounter)(test)
        val expected = Monad[Option].bind2(first, locById)((first1, locById1) => otherTest(first1._2, first1._1, locById1))
        result == expected
      }
      // I don't think this is easy to support for now cuz of issues with unapply in match statement
      // reminder: value pattern match is transformed into a pattern match
      /*"with original val pattern match" ! prop { (phone: Option[String], hitCounter: Option[String], locById: Option[String]) =>
        def test(a: String, b: String): Option[(String, String)] = Some((a, b))
        def otherTest(a: String, b: String, c: String): Option[String] = Some(a)

        val result = IdiomBracket.monad[Option, String] {
          val (dict, res) = extract(test(extract(phone), extract(hitCounter)))
          extract(otherTest(dict, res, extract(locById)))
        }
        val first = Monad[Option].bind2(phone, hitCounter)(test)
        val expected = Monad[Option].bind2(first, locById)((first1, locById1) => otherTest(first1._2, first1._1, locById1))
        result == expected
      }*/
    }
    "with interpolated string" in {
      "simple" ! prop {(a: Option[String]) =>
        val f = IdiomBracket[Option, String] {s"It’s ${extract(a)}!"}
        f ==== Applicative[Option].map(a)(aa => s"It’s $aa!")
      }
      "less simple" ! prop {(a: Option[String]) =>

        case class Ok(message: String)
        def nameOfMonth(num: Int): Option[String] = a
        val month = 5

        val f = IdiomBracket[Option, Ok]{
          Ok(s"It’s ${extract(nameOfMonth(month.toInt))}!")
        }

        if (a.isDefined)
          f == Some(Ok(s"It’s ${nameOfMonth(month.toInt).get}!"))
        else
          f == None
      }
    }
    "with currying" in {
      "2 currys with one param" ! prop { (a: Option[String], test: String => String => String, c: String) =>
        val f = IdiomBracket[Option, String](test(extract(a))(c))
        f == Applicative[Option].map(a)(test(_)(c))
      }
      "2 currys with one two params" ! prop { (a: Option[String], b: Option[String], test: (String, String) => String => String) =>
        val f = IdiomBracket[Option, String](test(extract(a), extract(b))("foo"))
        f == Applicative[Option].apply2(a, b)(test(_,_)("foo"))
      }
      "2 currys with two two params" ! prop { (a: Option[String], b: Option[String], test: (String, String) => (String, String) => String) =>
        val f = IdiomBracket[Option, String](test(extract(a), extract(b))("foo", "bar"))
        f == Applicative[Option].apply2(a, b)(test(_,_)("foo", "bar"))
      }
      "2 currys with extracts in both" ! prop { (a: Option[String], b: Option[String], test: String => String => String) =>
        val f = IdiomBracket[Option, String](test(extract(a))(extract(b)))
        f ==== Applicative[Option].apply2(a, b)(test(_)(_))
      }
     "2 currys with implicit" ! prop { (a: Option[String], b: String => String, c: String) =>
        trait Proof
        def test(fst: String)(implicit snd: Proof): String = b(fst)
        implicit val myProof = new Proof {}
        val f = IdiomBracket[Option, String](test(extract(a)))
        f ==== Apply[Option].map(a)(test)
      }
    }
    "with tuples" ! prop { (a: Option[String], b: Option[String], test: String => String) =>
      val f = IdiomBracket[Option,(String,String)]{(test(extract(a)), extract(b))}
      f ==== Applicative[Option].apply2(a,b)((aa,bb) => (test(aa),bb))
    }
    "with typed" in {
      "simple" ! prop { a: Option[String] =>
        val f = IdiomBracket[Option, String](extract(a: Option[String]))
        f ==== a
      }
      "complex" ! prop { (a: Option[String], test: String => String) =>
        val f = IdiomBracket[Option, String](test(extract(a)): String)
        f ==== a.map(test)
      }
    }
    "with List" ! prop {(a: List[String], b: List[String]) =>
      val f = IdiomBracket[List, String](extract(a) + extract(b))
      f == Applicative[List].apply2(a,b)(_ + _)
    }
    "with Future" ! prop {(a: scala.concurrent.Future[String], b: scala.concurrent.Future[String]) =>
      import scala.concurrent.Future
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration._

      implicit val applicative: Applicative[Future] = scalaz.std.scalaFuture.futureInstance

      val f = IdiomBracket[Future, String](extract(a) + extract(b))

      val timeout = FiniteDuration(100, TimeUnit.MILLISECONDS)
      Await.result(f, timeout) ==== Await.result(Applicative[Future].apply2(a,b)(_ + _), timeout)
    }
  }
}
