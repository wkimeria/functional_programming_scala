package chapter_4

import scala.annotation.tailrec

trait Option[+A] {

  /*
  * Exercise 4.1
   */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(s) => Some(s)
  }

  //def orElse[B >: A](ob: => Option[B]): Option[B]
  //def filter(f: A => Boolean): Option[A]
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
  }
}