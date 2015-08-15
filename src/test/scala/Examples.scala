package com.github.jedesah

import java.util.concurrent.{TimeoutException, TimeUnit}

import Expression.extract
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.specs2.ScalaCheck
import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try
import scalaz.std.scalaFuture.futureInstance
import scalaz._
import scalaz.syntax.writer._
import scalaz.std.string._

import scala.concurrent.ExecutionContext.Implicits.global

class Examples extends Specification with ScalaCheck {

  implicit def FutureArbitrary[A: Arbitrary]: Arbitrary[scala.concurrent.Future[A]] =
    Arbitrary(arbitrary[A] map ((x: A) => scala.concurrent.Future.successful(x)))

  implicit val phoneArbitrary: Arbitrary[Phone] =
    Arbitrary(arbitrary[String] map ((x: String) => Phone(x)))

  implicit val addressArbitrary: Arbitrary[Address] =
    Arbitrary(arbitrary[String] map ((x: String) => Address(x)))

  case class Phone(repr: String)
  case class Address(repr: String)
  type Score = Int
  type HTML = String

  "Examples" >> {
    "features" >> {
      "flexible use of abstractions" in {
        val combine = (a: Int, b: Int, c: Int) => a + b + c
        "the for comprehension version does not fail-fast" in {
          val a: Future[Int] = Future{Thread.sleep(1000); 10}
          val b: Future[Int] = Future.successful(2)
          val c: Future[Int] = Future{Thread.sleep(10); throw new Exception}
          val result: Future[Int] = for (aa <- a; bb <- b; cc <- c) yield combine(aa,bb,cc)
          Await.ready(result, 500.milliseconds) should throwA[TimeoutException]
        }
        "the Expression version does fail-fast" in {
          import Expression.auto.extract
          val a: Future[Int] = Future{Thread.sleep(1000); 10}
          val b: Future[Int] = Future.successful(2)
          val c: Future[Int] = Future{Thread.sleep(10); throw new Exception}
          val result: Future[Int] = Expression[Future, Int]{ combine(a, b, c) }
          // This will throw an exception if we are stuck waiting for the first Future
          Await.ready(result, 500.milliseconds)
          ok( """Future terminated before timeout which indicates "failing fast"""")
        }
      }
      "Playing well with if statements and match statements" in {
        "the naive for comprehension version is not smart about which Future's it actually needs" in {
          val a: Future[String] = Future.successful("Hello")
          val b: Future[Int] = Future{Thread.sleep(1000); 10}
          val c: Future[Int] = Future{Thread.sleep(10); 8}
          val polish = (a: Int) => a * 2
          val result: Future[Int] = for {
            aa <- a
            bb <- b
            cc <- c
          } yield if (aa == "something") polish(bb) else polish(cc)
          Await.ready(result, 500.milliseconds) should throwA[TimeoutException]
        }
        "the ceremonious version works but does not afford much over using the abtraction directly" in {
          val a: Future[String] = Future.successful("Hello")
          val b: Future[Int] = Future{Thread.sleep(1000); 10}
          val c: Future[Int] = Future{Thread.sleep(10); 8}
          val polish = (a: Int) => a * 2
          val result: Future[Int] = (for (aa <- a) yield
            if (aa == "something") for (bb <- b) yield polish(bb)
            else for (cc <- c) yield polish(cc)).flatMap(identity)
          Await.ready(result, 500.milliseconds)
          ok("Future terminated before timeout which means we did not wait on b")
        }
        "the Expression version works without ceremony" in {
          import Expression.auto.extract
          val a: Future[String] = Future.successful("Hello")
          val b: Future[Int] = Future{Thread.sleep(1000); 10}
          val c: Future[Int] = Future{Thread.sleep(10); 8}
          val polish = (a: Int) => a * 2
          val result: Future[Int] = Expression[Future,Int](if (extract(a) == "something") polish(b) else polish(c))
          Await.ready(result, 500.milliseconds)
          ok("Future terminated before timeout which means we did not wait on b")
        }
      }
      "Manipulating Context" in {
        "Writer" in {
          "for-comprehension version works but is less elegant" in {
            def random() = 0
            val aString = "This is a magic value"
            val bString = "I got this value from a dirty function"
            val cString = "Hello World..."
            val ctxString = "We avoided a division by zero, yay!"
            val a: Writer[String, Int] = 8.set(aString)
            val b: Writer[String, Int] = random().set(bString)
            val c: Writer[String, String] = "Hello World".set(cString)
            val result = for {
              aa <- a
              bb <- b
              div <- if (bb == 0) {
                5.set(ctxString)
              } else Writer("", aa / bb)
              cc <- c
            } yield cc * div
            result ==== Writer(aString + bString + ctxString + cString, "Hello World" * 5)
          }
          "Expression version" in {
            pending
          }
        }
        "Future" in {
          "for-comprehension version" in {
            val a: Future[Int] = Future.successful(4)
            val b: Future[Int] = Future.successful(0)
            val c: Future[String] = Future.successful("Hello World")
            val result = for {
              aa <- a
              bb <- b
              div <- if (bb == 0) {
                Future.failed(new Exception("Division by zero :-("))
              } else Future.successful(aa / bb)
              cc <- c
            } yield cc * div;
            // Await.result(result, 1.second) should throwAn[Exception] does not work for some reason...
            Try(Await.result(result, 1.second)).isFailure ==== true
          }
          "Expression version" in {
            pending
          }
        }
      }
    }
    "Usage" in {
      "explicit" ! prop { (phoneString: String,
                           lookupPhone: String => Future[Phone],
                           lookupAddress: Phone => Future[Address],
                           lookupReputation: Phone => Future[Score],
                           renderPage: (Phone, Address, Int) => HTML) =>

        val f: Future[HTML] = Expression.monad[Future, HTML] {
          val phone = lookupPhone(phoneString)
          val address = lookupAddress(extract(phone))
          val rep = lookupReputation(extract(phone))
          renderPage(extract(phone), extract(address), extract(rep))
        }
        val expected = {
          val phone = lookupPhone(phoneString)
          val address = Monad[Future].bind(phone)(lookupAddress)
          val rep = Monad[Future].bind(phone)(lookupReputation)
          Applicative[Future].apply3(phone, address, rep)(renderPage)
        }
        val timeout = FiniteDuration(100, TimeUnit.MILLISECONDS)
        Await.result(f, timeout) ==== Await.result(expected, timeout)
      }
      "implicit" ! prop { (phoneString: String,
                           lookupPhone: String => Future[Phone],
                           lookupAddress: Phone => Future[Address],
                           lookupReputation: Phone => Future[Score],
                           renderPage: (Phone, Address, Int) => HTML) =>
        import com.github.jedesah.Expression.auto.extract
        val f: Future[HTML] = Expression.monad[Future, HTML] {
          val phone = lookupPhone(phoneString)
          val address = lookupAddress(phone)
          val rep = lookupReputation(phone)
          renderPage(phone, address, rep)
        }
        val expected = {
          val phone = lookupPhone(phoneString)
          val address = Monad[Future].bind(phone)(lookupAddress)
          val rep = Monad[Future].bind(phone)(lookupReputation)
          Applicative[Future].apply3(phone, address, rep)(renderPage)
        }
        val timeout = FiniteDuration(100, TimeUnit.MILLISECONDS)
        Await.result(f, timeout) ==== Await.result(expected, timeout)
      }
    }
  }
}
