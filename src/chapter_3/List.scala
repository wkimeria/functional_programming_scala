package chapter_3

import scala.annotation.tailrec

sealed trait List[+a]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object Tester {
  def main(args: Array[String]): Unit = {
    assert(List.sum(List(1, 2, 3)) == 6)
    assert(List.product(List(1, 2, 3, 4)) == 24)
    assert(List.sum2(List(1, 2, 3)) == 6)
    assert(List.product2(List(1, 2, 3, 4)) == 24)
    assert(List.setHead(List(1, 2, 3), 9) == Cons(9, Cons(2, Cons(3, Nil))))
    assert(List.drop(List(1, 2, 3, 4), 2) == Cons(3, Cons(4, Nil)))
    def even(v: Int): Boolean = {
      v % 2 == 0
    }
    assert(List.dropWhile(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))), even) == Cons(1, Cons(3, Cons(5, Nil))))
    assert(List.init(List(1, 2, 3, 4, 5)) == Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
    assert(List.length(List(1, 2, 3, 4, 5)) == 5)
  }
}

object List {

  def foldRight[A,B](as: List[A], z: B)(f:(A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Int]) =
    foldRight(ns, 1.0)(_ *_)

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /*
  Exercise 3.2
  Implement the function tail for removing the first element of a List.
  Note that the function takes constant time. What are different choices you could make in your implementation if
  the List is Nil?
  */
  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => lst
    case Cons(h, t) => t
  }

  /*
  Exercise 3.3
  Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
  */
  def setHead[A](lst: List[A], i: A): List[A] = lst match {
    case Nil => lst
    case Cons(h, t) => Cons(i, t)
  }

  /*
  Exercise 3.4
  Generalize tail to the function drop, which removes the first n elements from a list. Note that this function
  takes time proportional only to the number of elements being dropped—we don’t need to make a copy of the entire List

  def drop[A](l: List[A], n: Int): List[A]
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def loop(lst: List[A], counter: Int): List[A] = {
      if (counter == n)
        lst
      else {
        val dropped = tail(lst)
        loop(dropped, counter + 1)
      }
    }
    loop(l, 0)
  }

  /*
  Exercise 3.5
  Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.

  def dropWhile[A](l: List[A], f: A => Boolean): List[A]
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {

    //TODO: Is there a way of writing this in a Tail Recursive way? @tailrec
    //TODO: Revisit, there is a better way of doing this.
    def loop(lst: List[A]): List[A] = {
      lst match {
        case Nil => lst
        case Cons(h, t) => {
          if (f(h) == true)
            loop(t)
          else
            Cons(h, loop(t))
        }
      }
    }
    loop(l)
  }

  /*
  Exercise 3.6
  Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but the
  last element of a List. So, given List(1,2,3,4), init will return List(1,2,3).
  Why can’t this function be implemented in constant time like tail?

  def init[A](l: List[A]): List[A]
   */
  def init[A](l: List[A]): List[A] = {

    @annotation.tailrec
    def loop(lst: List[A], acc: List[A]): List[A] = {
      lst match {
        case Cons(x, Nil) => acc
        case Cons(y, z) => loop(z, Cons(y, acc))
      }
    }
    @annotation.tailrec
    def reverse(lst: List[A], acc: List[A]): List[A] = {
      lst match {
        case Cons(x, Nil) => Cons(x, acc)
        case Cons(y, z) => reverse(z, Cons(y, acc))
      }
    }
    reverse((loop(l, List())), List())
  }

  /*
  Exercise 3.7 (No code, just reasoning through the problem)
  Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
  Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
  This is a deeper question that we’ll return to in chapter 5.
   */

  /*
  Exercise 3.8 (No code, just reasoning through the problem)
  See what happens when you pass Nil and Cons themselves to foldRight, like this:
  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).[10]
  What do you think this says about the relationship between foldRight and the data constructors of List?
   */

  /*
  Exercise 3.9
  Compute the length of a list using foldRight.
  def length[A](as: List[A]): Int
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => 1 + y)
  }


  /*
  Exercise 3.10
  Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists
  (we say it’s not stack-safe).
  Convince yourself that this is the case, and then write another general list-recursion function, foldLeft,
  that is tail-recursive,using the techniques we discussed in the previous chapter. Here is its signature:

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B

  [note]
  Again, foldLeft is defined as a method of List in the Scala standard library, and it is curried similarly
  for better type inference, so you can write mylist.foldLeft(0.0)(_ + _).
   */

}