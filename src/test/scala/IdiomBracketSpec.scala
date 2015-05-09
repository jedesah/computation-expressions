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

class IdiomBracketSpec extends Specification with ScalaCheck {

  implicit def FutureArbitrary[A: Arbitrary]: Arbitrary[scala.concurrent.Future[A]] =
    Arbitrary(arbitrary[A] map ((x: A) => scala.concurrent.Future.successful(x)))

  type ThirteenOptions[A] = Option[A] ::
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

  "IdiomBracket" >> {
    "function application" >> {
      "2 params" ! prop { (a: Option[String], b: Option[String], doThing: (String, String) => String) =>
        val f = Expression.idiom[Option, String](doThing(extract[Option, String](a), extract[Option, String](b)))
        f ==== Apply[Option].apply2(a, b)(doThing)
      }
      "3 params" >> {
        "all of them extracts" ! prop { (a: Option[String], b: Option[String], c: Option[String], doThing: (String, String, String) => String) =>
          val f = Expression.idiom[Option, String](doThing(extract(a), extract(b), extract(c)))
          f ==== Applicative[Option].apply3(a, b, c)(doThing)
        }
        "some extracts, some not" ! prop { (a: String, b: Option[String], c: Option[String], doThing: (String, String, String) => String) =>
          val f = Expression.idiom[Option, String](doThing(a, extract(b), extract(c)))
          f ==== Applicative[Option].apply3(Some(a), b, c)(doThing)
        }
        //        "so many parameters" ! prop {
        //          (args: ThirteenOptions[String],
        //            fun: (String, String, String, String, String, String, String, String, String, String, String, String, String) => String) =>
        //            val (a,b,c,d,e,f,g,h,i,j,k,l,m) = args.tupled
        //            val result = IdiomBracket[Option, String](fun(
        //              extract(a),
        //              extract(b),
        //              extract(c),
        //              extract(d),
        //              extract(e),
        //              extract(f),
        //              extract(g),
        //              extract(h),
        //              extract(i),
        //              extract(j),
        //              extract(k),
        //              extract(l),
        //              extract(m)))
        //            result ==== Applicative[Option].map(sequence(a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: HNil)){
        //              case aa :: bb :: cc :: dd :: ee :: ff :: gg :: hh :: ii :: jj :: kk :: ll :: mm :: HNil =>
        //                fun(aa,bb,cc,dd,ee,ff,gg,hh,ii,jj,kk,ll,mm)
        //            }
        //        }
      }
      tag("type parameter")
      "function with type parameters" ! prop { (a: Option[String], fooImpl: String => String, b: String) =>
        def foo[A](a: A, b: String): String = fooImpl(b) + a
        val f = Expression.idiom[Option, String](foo(b, extract(a)))
        f ==== Applicative[Option].map(a)(foo(b, _))
      }
    }
    "method invocation" in {
      "no extract in LHS" ! prop { (a: String, b: Option[Int], c: Option[Int]) =>
        val f = Expression.idiom[Option, Int](a.indexOf(extract(b), extract(c)))
        f ==== Applicative[Option].apply2(b, c)(a.indexOf(_, _))
      }
      "extract in LHS" ! prop { (a: Option[String], b: Int, c: Option[Int]) =>
        val f = Expression.idiom[Option, Int](extract(a).indexOf(b, extract(c)))
        f ==== Applicative[Option].apply2(a, c)(_.indexOf(b, _))
      }
      "complex method invocation" in {
        "1" ! prop { (a: Option[String], b: Int, c: Option[Int], doThing: (String, String) => String) =>
          val f = Expression.idiom[Option, Int](doThing(extract(a), extract(c).toString).indexOf(b, extract(c)))
          f ==== Applicative[Option].apply2(a, c)((aa, cc) => doThing(aa, cc.toString).indexOf(b, cc))
        }
        "2" ! prop { (a: Option[String], b: Int, c: Option[Int], d: Option[String], doThing: (String, String) => String) =>
          val f = Expression.idiom[Option, Int](doThing(extract(a), extract(d)).indexOf(b, extract(c)))
          f ==== Applicative[Option].apply3(a, c, d)((aa, cc, dd) => doThing(aa, dd).indexOf(b, cc))
        }
      }
      tag("type parameter")
      "with type parameters" ! prop { (a: Option[Either[String, Int]]) =>
        val f = Expression.idiom[Option, Int](extract(a).fold(_.length, identity))
        f ==== Applicative[Option].map(a)(_.fold(_.length, identity))
      }
    }
    "extract buried" in {
      "deep" ! prop { (a: Option[String], b: Option[String], c: Option[String], doThing: (String, String, String) => String, otherThing: String => String) =>
        val f = Expression.idiom[Option, String](doThing(otherThing(extract(a)), extract(b), extract(c)))
        f ==== Applicative[Option].apply3(a, b, c)((aa, bb, cc) => doThing(otherThing(aa), bb, cc))
      }
      "deeper" ! prop { (a: Option[String], b: Option[String], c: Option[String], doThing: (String, String, String) => String, otherThing: String => String, firstThis: String => String) =>
        val f = Expression.idiom[Option, String](doThing(otherThing(firstThis(extract(a))), extract(b), extract(c)))
        f ==== Applicative[Option].apply3(a, b, c)((aa, bb, cc) => doThing(otherThing(firstThis(aa)), bb, cc))
      }
    }
    tag("block")
    "block" in {
      "simple" ! prop { (a: Option[String], otherThing: String => String) =>
        val f = Expression.idiom[Option, String] {
          otherThing(extract(a))
        }
        f ==== Applicative[Option].map(a)(otherThing)
      }
      "slighly more complex is a useless way you would think" ! prop { (a: Option[String], otherThing: String => String) =>
        val f = Expression.idiom[Option, String] {
          otherThing(extract(a))
          otherThing(extract(a))
        }
        f ==== Applicative[Option].map(a)(otherThing)
      }
      "pointless val" ! prop { (a: Option[String], otherThing: String => String) =>
        val f = Expression.idiom[Option, String] {
          val aa = otherThing(extract(a))
          aa
        }
        f ==== Applicative[Option].map(a)(otherThing)
      }
      "slightly less simple and somewhat useful" ! prop { (a: Option[String], otherThing: String => String) =>
        val f = Expression.idiom[Option, String] {
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
      "match" in {
        "with extract in expression" ! prop { (a: Option[String]) =>
          val f = Expression.idiom[Option, String] {
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
          val f = Expression.idiom[Option, String] {
            List(1, 2, 3) match {
              case Nil => extract(a) + "!"
              case _ => "hello"
            }
          }
          val expected = List(1, 2, 3) match {
            case Nil => Applicative[Option].map(a)(_ + "!")
            case _ => Applicative[Option].pure("hello")
          }
          f ==== expected
        }
        "with stable identifier in  pattern match case statement" ! prop { (a: Option[String], b: Option[String]) =>
          val f = Expression.idiom[Option, String] {
            val bb = extract(b)
            extract(a) match {
              case `bb` => "h"
              case _ => "e"
            }
          }
          val expected = Applicative[Option].apply2(a, b)((a, b) =>
            a match {
              case `b` => "h"
              case _ => "e"
            }
          )
          f == expected
        }
      }
      "if statement" in {
        "extract in condition expression" ! prop { (a: Option[String]) =>
          val f = Expression.idiom[Option, Int] {
            if (extract(a).length == 5) 10 else 20
          }
          f ==== Applicative[Option].map(a)(aa => if (aa.length == 5) 10 else 20)
        }
      }
      tag("funky")
      "renamed import" ! prop { (a: Option[String], b: Option[String], doThing: (String, String) => String) =>
        import Expression.{extract => extractt}
        val f = Expression.idiom[Option, String](doThing(extractt(a), extractt(b)))
        f == Applicative[Option].apply2(a, b)(doThing)
      }
      tag("funky")
      "implicit extract" ! prop { (a: Option[String], b: Option[String], doThing: (String, String) => String) =>
        import Expression.auto.extract
        val f = Expression.idiom[Option, String](doThing(a, b))
        f == Applicative[Option].apply2(a, b)(doThing)
      }
      "with interpolated string" in {
        "simple" ! prop { (a: Option[String]) =>
          val f = Expression.idiom[Option, String] {
            s"It’s ${extract(a)}!"
          }
          f ==== Applicative[Option].map(a)(aa => s"It’s $aa!")
        }
        "less simple" ! prop { (a: Option[String]) =>

          case class Ok(message: String)
          def nameOfMonth(num: Int): Option[String] = a
          val month = 5

          val f = Expression.idiom[Option, Ok] {
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
          val f = Expression.idiom[Option, String](test(extract(a))(c))
          f == Applicative[Option].map(a)(test(_)(c))
        }
        "2 currys with one two params" ! prop { (a: Option[String], b: Option[String], test: (String, String) => String => String) =>
          val f = Expression.idiom[Option, String](test(extract(a), extract(b))("foo"))
          f == Applicative[Option].apply2(a, b)(test(_, _)("foo"))
        }
        "2 currys with two two params" ! prop { (a: Option[String], b: Option[String], test: (String, String) => (String, String) => String) =>
          val f = Expression.idiom[Option, String](test(extract(a), extract(b))("foo", "bar"))
          f == Applicative[Option].apply2(a, b)(test(_, _)("foo", "bar"))
        }
        "2 currys with extracts in both" ! prop { (a: Option[String], b: Option[String], test: String => String => String) =>
          val f = Expression.idiom[Option, String](test(extract(a))(extract(b)))
          f ==== Applicative[Option].apply2(a, b)(test(_)(_))
        }
        "2 currys with implicit" ! prop { (a: Option[String], b: String => String, c: String) =>
          trait Proof
          def test(fst: String)(implicit snd: Proof): String = b(fst)
          implicit val myProof = new Proof {}
          val f = Expression.idiom[Option, String](test(extract(a)))
          f ==== Apply[Option].map(a)(test)
        }
      }
      "with tuples" ! prop { (a: Option[String], b: Option[String], test: String => String) =>
        val f = Expression.idiom[Option, (String, String)] {
          (test(extract(a)), extract(b))
        }
        f ==== Applicative[Option].apply2(a, b)((aa, bb) => (test(aa), bb))
      }
      "with typed" in {
        "simple" ! prop { a: Option[String] =>
          val f = Expression.idiom[Option, String](extract(a: Option[String]))
          f ==== a
        }
        "complex" ! prop { (a: Option[String], test: String => String) =>
          val f = Expression.idiom[Option, String](test(extract(a)): String)
          f ==== a.map(test)
        }
      }
      "with List" ! prop { (a: List[String], b: List[String]) =>
        val f = Expression[List, String](extract(a) + extract(b))
        f == Applicative[List].apply2(a, b)(_ + _)
      }
      "with Future" ! prop { (a: scala.concurrent.Future[String], b: scala.concurrent.Future[String]) =>
        import scala.concurrent.Future
        import scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.duration._

        implicit val applicative: Applicative[Future] = scalaz.std.scalaFuture.futureInstance

        val f = Expression[Future, String](extract(a) + extract(b))

        val timeout = FiniteDuration(100, TimeUnit.MILLISECONDS)
        Await.result(f, timeout) ==== Await.result(Applicative[Future].apply2(a, b)(_ + _), timeout)
      }
    }
  }
}
