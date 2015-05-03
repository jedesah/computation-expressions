package com.github.jedesah

import java.util.concurrent.TimeUnit

import org.specs2.mutable._

import scala.concurrent.Await
import scalaz.Monad

class Assumptions extends Specification {

  "assumptions" should {
    "future" in {
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

      val f = Monad[Future].bind(a)(if(_) b else c)

      f.value ==== None

      aPromise.success(true)

      f.value ==== None

      bPromise.success("hello")

      Await.result(f, FiniteDuration(1, TimeUnit.SECONDS)) ==== "hello"
    }
  }
}