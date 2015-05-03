package com.github.jedesah

import java.util.concurrent.TimeUnit

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.specs2.ScalaCheck
import org.specs2.mutable._
import Expression.{extract, bind2, bind3}
import shapeless._

import scala.concurrent.Await
import scalaz.{Monad, Applicative, Apply}
import scalaz.std.option._
import scalaz.std.list._
import shapeless.contrib.scalaz._

class ExpressionSpec extends Specification with ScalaCheck {

  implicit def FutureArbitrary[A: Arbitrary]: Arbitrary[scala.concurrent.Future[A]] =
    Arbitrary(arbitrary[A] map ((x: A) => scala.concurrent.Future.successful(x)))

  type ThirteenOptions[A] =  Option[A] ::
                          Option[A] ::
                          Option[A] ::
                          Option[A] ::
                          Option[A] ::
                          Option[A] ::
                          Option[A] ::
                          Option[A] ::
                          Option[A] ::
                          Option[A] ::
                          Option[A] ::
                          Option[A] ::
                          Option[A] :: HNil

  "Expression" should {
    "double nested extract within argument" in {
      "simple enough" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int], doThing: (String, String, String) => String, firstThis: String => Option[String]) =>
        val f = Expression.monad[Option, String](doThing(extract(firstThis(extract(a))), extract(b), extract(c)))
        f ==== Applicative[Option].apply3(Monad[Option].bind(a)(firstThis), b, c)(doThing)
      }
      "nested a little deeper" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int], doThing: (String, String, String) => String, firstThis: String => Option[String], other: String => String) =>
        val f = Expression.monad[Option, String](doThing(other(extract(firstThis(extract(a)))), extract(b), extract(c)))
        f ==== Applicative[Option].apply3(Applicative[Option].map(Monad[Option].bind(a)(firstThis))(other), b, c)(doThing)
      }
      "with 2 monads inside first extract" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int],
                                                     doThing: (String, String) => String,
                                                     firstThis: (String, String) => Option[String],
                                                     other: String => String) =>
        val f = Expression.monad[Option, String](doThing(other(extract(firstThis(extract(a), extract(b)))), extract(c)))
        f == Applicative[Option].apply2(
          Applicative[Option].map(bind2(a, b)(firstThis))(other), c
        )(doThing)
      }
      "tricky function that takes a monad and extracts itself. Want to make sure we are not to eager to lift things" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int], doThing: (String, String, String) => String, firstThis: String => String, other: String => String) =>
        val f = Expression.monad[Option, String](doThing(other(firstThis(extract(a))), extract(b), extract(c)))
        f ==== Applicative[Option].apply3(Applicative[Option].map(Applicative[Option].map(a)(firstThis))(other), b, c)(doThing)
      }
      "if/else expression that is like a monadic function" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int], doThing: (String, String, String) => String, other: String => String) =>
        val f = Expression.monad[Option, String] {
          doThing(other(extract(if (extract(a) == "") Some("a") else None)), extract(b), extract(c))
        }
        f ==== Applicative[Option].apply3(Applicative[Option].map(Monad[Option].bind(a)(f => if (f == "") Some("a") else None))(other), b, c)(doThing)
      }
      "interpolated String" ! prop { (a: Option[String]) =>
        val f = Expression.monad[Option, String] {
          s"It is ${extract(a)}!"
        }
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

      val f = Expression.monad[Future, String](if (extract(a)) extract(b) else extract(c))

      f.value ==== None

      aPromise.success(true)

      f.value ==== None

      bPromise.success("hello")

      Await.result(f, FiniteDuration(1, TimeUnit.SECONDS)) ==== "hello"
    }
    tag("match")
    "with match (iGraph example)" ! prop { (namesOption: Option[Map[String, String]], userLocale: String) =>
      sealed trait LocationType
      case class City(city: Option[String]) extends LocationType
      case class Country(countryName: Option[String]) extends LocationType
      case object State extends LocationType
      val loc: LocationType = City(None)
      // 'EN-US' will be "normalized" to 'en'
      def normalize(locale: String) = locale.take(2).toLowerCase
      val f = Expression.monad[Option, LocationType] {
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
    tag("block")
    "block" in {
      "1" ! prop { (a: Option[String], foo: String => Option[String], bar: String => String) =>
        val f = Expression.monad[Option, String] {
          val b = foo(extract(a))
          bar(extract(b))
        }
        f ==== Monad[Option].bind(a)(aa => Applicative[Option].map(foo(aa))(bar))
      }
      "2" ! prop { (a: Option[String], d: Option[String], foo: String => Option[String], bar: (String, String) => String, biz: String => Option[String]) =>
        val f = Expression.monad[Option, String] {
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
    tag("match")
    "match" in {
      "simple" ! prop { (a: Option[String], b: Option[String], c: Option[String], foo: String => String, bar: String => String) =>
        val f = Expression.monad[Option, String] {
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
        val f = Expression.monad[Option, String] {
          val dd = extract(d)
          extract(a) match {
            case `dd` => foo(extract(b))
            case _ => bar(extract(c))
          }
        }
        val expected = bind2(a, d) { (aa, dd) => aa match {
          case `dd` => Apply[Option].map(b)(foo)
          case _ => Apply[Option].map(c)(bar)
        }
        }
        f ==== expected
      }.pendingUntilFixed("Not yet implemented to take advantage of Monad")
      "with multiple stable identifier in the pattern matches of case statements" ! prop {
        (a: Option[String],
         b: Option[String],
         c: Option[String],
         d: Option[String],
         e: Option[String],
         f: String,
         foo: String => String,
         bar: String => String) =>
        val result = Expression.monad[Option, String] {
          val dd = extract(d)
          val ee = extract(e)
          extract(a) match {
            case `dd` => foo(extract(b))
            case `ee` => bar(extract(c))
            case _ => f
          }
        }
        val expected = bind2(a, d) { (aa, dd) => aa match {
          case `dd` => Apply[Option].map(b)(foo)
          case _ => Apply[Option].map(c)(bar)
        }
        }
        result ==== expected
      }.pendingUntilFixed("Not yet implemented")
    }
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

      val result = Expression.monad[Option, String] {
        val tuple: (String, String) = extract(test(extract(phone), extract(hitCounter)))
        extract(otherTest(tuple._2, tuple._1, extract(locById)))
      }
      val first = bind2(phone, hitCounter)(test)
      val expected = bind2(first, locById)((first1, locById1) => otherTest(first1._2, first1._1, locById1))
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
}
