package chapter_4

import scala.annotation.tailrec

trait Option[+A] {

  /*
  * Exercise 4.1
  * It’s fine to use pattern matching, though you should be able to implement all the functions besides map and
  * getOrElse without resorting to pattern matching.
  *
  * For map and flatMap, the type signature should be enough to determine the implementation.
  *
  * getOrElse returns the result inside the Some case of the Option, or if the Option is None,
  * returns the given default value.
  *
  * orElse returns the first Option if it’s defined; otherwise, it returns the second Option.
   */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
}


case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Tester {
  def main(args: Array[String]): Unit = {
    assert(Some(1).map((v) => v * 30) == Some(30))
    assert(Some(1).flatMap((v) => Some(v * 30)) == Some(30))
    assert(None.getOrElse(-100) == -100)
    assert(Some(33).getOrElse(-100) == 33)
    assert(None.orElse(Some(-100)) == Some(-100))
    assert(Some(33).orElse(Some(-100)) == Some(33))
    assert(Some(4).filter((x) => (x % 2 == 0)) == Some(4))
    assert(Some(5).filter((x) => (x % 2 == 0)) == None)
    assert(variance(List(2.0, 4.0, 6.0)) == Some(2.6666666666666665))
    println(variance(List(2.0, 4.0, 6.0)))

  }

 /*
 * Exercise 4.2
 * Implement the variance function in terms of flatMap.
 * If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
 * See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
 *
 * def variance(xs: Seq[Double]): Option[Double]
 */
  def variance(xs: Seq[Double]): Option[Double] = {
    //TODO: Review this
    //mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
    def mean(x: Seq[Double]): Double = x.sum / x.length
    Some(xs.flatMap((v) => List(math.pow((v - mean(xs)), 2))).sum / xs.length)
  }
}