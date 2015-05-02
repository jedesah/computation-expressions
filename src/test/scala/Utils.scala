package com.github.jedesah

package object utils {
  trait Range
  case class Last(i: Int) extends Range
  case class Take(i: Int) extends Range
  case class Drop(i: Int) extends Range
  case class Slice(from: Int, to: Int) extends Range
  implicit class AugmentedSeq[A](seq: Seq[A]) {
    def slice(range: Range): Seq[A] = {
      range match {
        case Take(i) => seq.take(i)
        case Drop(i: Int) => seq.drop(i)
        case Last(i) => seq.takeRight(i)
        case Slice(from, to) => seq.slice(from, to)
      }
    }
  }
}
