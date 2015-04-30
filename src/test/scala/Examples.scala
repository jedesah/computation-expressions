package com.github.jedesah

import java.util.concurrent.TimeUnit

import com.github.jedesah.IdiomBracket.extract
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.specs2.ScalaCheck
import org.specs2.mutable._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scalaz.std.scalaFuture.futureInstance
import scalaz.{Applicative, Monad}

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

  "Examples" should {
    "simple enough" in {
      "explicit" ! prop { (phoneString: String,
                           lookupPhone: String => Future[Phone],
                           lookupAddress: Phone => Future[Address],
                           lookupReputation: Phone => Future[Score],
                           renderPage: (Phone, Address, Int) => HTML) =>

        val f: Future[HTML] = IdiomBracket.monad[Future, HTML] {
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
        import com.github.jedesah.IdiomBracket.auto.extract
        val f: Future[HTML] = IdiomBracket.monad[Future, HTML] {
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
