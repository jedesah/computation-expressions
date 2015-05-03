package com.github.jedesah

import java.util.concurrent.TimeUnit

import com.github.jedesah.Expression._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.specs2.ScalaCheck
import org.specs2.mutable._
import shapeless._

import scala.concurrent.Await
import scalaz.{Applicative, Apply, Monad}

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
    "support Monad when there is a Monad instance available" ! prop { (a: Option[String], b: Option[String], c: Option[String], d: Option[Int], doThing: (String, String, String) => String, firstThis: String => Option[String]) =>
      import scalaz.std.option.optionInstance
      val f = Expression[Option, String](doThing(extract(firstThis(extract(a))), extract(b), extract(c)))
      f ==== Applicative[Option].apply3(Monad[Option].bind(a)(firstThis), b, c)(doThing)
    }
    "only require Applicative when the Monad typeclass is not required" ! prop { (a: Option[String], b: Option[String], doThing: (String, String) => String) =>
      implicit val instance: Applicative[Option] = scalaz.std.option.optionInstance
      val f = Expression[Option, String](doThing(extract[Option, String](a), extract[Option, String](b)))
      f ==== Apply[Option].apply2(a, b)(doThing)
    }
  }
}
