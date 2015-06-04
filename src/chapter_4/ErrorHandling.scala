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
    println("Running Tests for Chapter 4 ....")
    assert(Some(1).map((v) => v * 30) == Some(30))
    assert(Some(1).flatMap((v) => Some(v * 30)) == Some(30))
    assert(None.getOrElse(-100) == -100)
    assert(Some(33).getOrElse(-100) == 33)
    assert(None.orElse(Some(-100)) == Some(-100))
    assert(Some(33).orElse(Some(-100)) == Some(33))
    assert(Some(4).filter((x) => (x % 2 == 0)) == Some(4))
    assert(Some(5).filter((x) => (x % 2 == 0)) == None)
    assert(variance(List(2.0, 4.0, 6.0)) == Some(2.6666666666666665))
    assert(variance2(List(2.0, 4.0, 6.0)) == Some(2.6666666666666665))
    assert(map2(Some(1), Some(4))((a, b) => a + b) == Some(5))
    //TODO: Why does the below not compile?
    //assert(map2(None, Some(4))((a, b) => a + b) == None)
    //assert(map2(None, None)((a, b) => a + b) == None)
    assert(map2(Some(1), None)((a, b) => a + b) == None)

  }

  def variance2(xs: Seq[Double]): Option[Double] = {
    def mean(x: Seq[Double]): Seq[Double] = {
      List(x.sum / x.length)
      //x.map((v) => x.sum / x.length)
    }
    val r = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
    Some(r(0))
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

  /*
  Exercise 4.3
  Write a generic function map2 that combines two Option values using a binary function.
  If either Option value is None, then the return value is too.
    Here is its signature:

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
  */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =(a, b) match {
      case (Some(v), Some(y)) => Some(f(v, y))
      case (_, None) => None
      case (None, _) => None
      case (None, None) => None
    }
}