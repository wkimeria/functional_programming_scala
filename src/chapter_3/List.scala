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
    assert(List.productRight(List(1, 2, 3, 4)) == 24)
    assert(List.setHead(List(1, 2, 3), 9) == Cons(9, Cons(2, Cons(3, Nil))))
    assert(List.drop(List(1, 2, 3, 4), 2) == Cons(3, Cons(4, Nil)))
    def even(v: Int): Boolean = {
      v % 2 == 0
    }
    assert(List.dropWhile(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))), even) == Cons(1, Cons(3, Cons(5, Nil))))
    assert(List.init(List(1, 2, 3, 4, 5)) == Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
    assert(List.length(List(1, 2, 3, 4, 5)) == 5)
    assert(List.sumLeft(List(1, 2, 3)) == 6)
    assert(List.productLeft(List(1, 2, 3, 4)) == 24)
    assert(List.lengthLeft(List(6, 2, 8, 4, 5, 33, 128, -200)) == 8)
    assert(List.reverse(List(2, 4, 6, 8)) == Cons(8, Cons(6, Cons(4, Cons(2, Nil)))))
    assert(List.append(List(1, 2, 3), List(4, 5)) == Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))
    assert(List.addOne(List(2, 4, 6, 8)) == Cons(3, Cons(5, Cons(7, Cons(9, Nil)))))
    assert(List.doubleToString(List(1, 2, 3, 4, 5)) == Cons("1", Cons("2", Cons("3", Cons("4", Cons("5", Nil))))))
    assert(List.map(List(1, 2, 3))((x) => x * x) == Cons(1, Cons(4, Cons(9, Nil))))
    assert(List.filterOld(List(1, 2, 3, 4, 5, 6))((x) => (x % 2) != 0) == Cons(2, Cons(4, Cons(6, Nil))))
    assert(List.flatMap(List(1, 2, 3, 4))(i => List(i, i)) == Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Cons(4, Cons(4, Nil)))))))))
    assert(List.filter(List(1, 2, 3, 4, 5, 6))((x) => (x % 2) != 0) == Cons(1, Cons(3, Cons(5, Nil))))
    assert(List.combine(List(1, 2, 3), List(4, 5, 6)) == Cons(5, Cons(7, Cons(9, Nil))))
    assert(List.zipWith(List(1.0, 2.0, 3.0), List(4.0, 5.0, 6.0))((a, b) => (a + b)) == Cons(5.0, Cons(7.0, Cons(9.0, Nil))))
    assert(List.productRightExitOnZero(List(1.0, 2.0, 3.0, 4.0)) == 24.0)
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) == true)
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4)) == true)
    assert(List.hasSubsequence(List(1, 2, 3, 1, 2, 3, 4), List(1, 2, 3, 4)) == true)
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(5, 6)) == false)
    assert(List.hasSubsequence(List(1, 2, 3, 4, 5), List(1, 4, 5)) == false)

    assert(Tree.treeSizeOld(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Tree.treeSizeOld(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(Nil)))) == 7)

    assert(Tree.treeSize(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Tree.treeSize(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(Nil)))) == 7)


    val l1 = Branch(Leaf(1), Leaf(4))
    val l2 = Branch(Leaf(55), Leaf(9))
    val l3 = Branch(l1, l2)
    val l4 = Branch(Leaf(101), Leaf(7))
    val l5 = Branch(Leaf(19), Leaf(-9))
    val l6 = Branch(l1, l2)
    val l7 = Branch(l4, l5)
    val l8 = Branch(l3, l7)

    assert(Tree.maximumOld(l8) == 101)

    assert(Tree.maximum(l8) == 101)

    val left1 = Leaf(1)
    val right1 = Leaf(2)
    val branch1 = Branch(left1, right1)

    val left2 = Leaf(3)
    val right2 = Leaf(4)
    val branch2 = Branch(left2, right2)

    val branch3 = Branch(branch1, branch2)

    val branch4 = Branch(branch3, Leaf(22))

    assert(Tree.depthOld(branch4) == 4)
    println(Tree.depth(branch4))
    assert(Tree.depth(branch4) == 4)

    val left3 = Leaf(3)
    val right3 = Leaf(4)
    val right3a = Branch(Leaf(7), Leaf(11))
    val branch5 = Branch(left3, right3)
    val branch6 = Branch(right3a, branch5)
    assert(branch6 == Branch(Branch(Leaf(7), Leaf(11)), Branch(Leaf(3), Leaf(4))))
    assert(Tree.mapOld(branch6, ((x: Int) => x * x)) == Branch(Branch(Leaf(49), Leaf(121)), Branch(Leaf(9), Leaf(16))))
    assert(Tree.map(branch6, ((x: Int) => x * x)) == Branch(Branch(Leaf(49), Leaf(121)), Branch(Leaf(9), Leaf(16))))

  }
}

object List {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def productRight(ns: List[Int]) =
    foldRight(ns, 1.0)(_ * _)

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
  //TODO:Write code here
  //TODO: Why can't this be type invariant
  def productRightExitOnZero(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)


  /*
  Exercise 3.8 (No code, just reasoning through the problem)
  See what happens when you pass Nil and Cons themselves to foldRight, like this:
  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).[10]
  What do you think this says about the relationship between foldRight and the data constructors of List?
   */
  //TODO: Write code here

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

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  /*
  Exercise 3.11
  Write sum, product, and a function to compute the length of a list using foldLeft.
   */
  def productLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0)((x, y) => x * y)

  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def lengthLeft[A](ns: List[A]) =
    foldLeft(ns, 0)((x, y) => x + 1)

  /*
  Exercise 3.12
  Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
  See if you can write it using a fold.
   */
  //Trick is realizing second input argument can be List of type x
  def reverse[A](ns: List[A]): List[A] = foldLeft(ns, List[A]())((lst, ns) => Cons(ns, lst))


  /*
  Exercise 3.13
  Hard: Can you write foldLeft in terms of foldRight? How about the other way around? Implementing foldRight via
  foldLeft is useful because it lets us implement foldRight tail-recursively, which means it works even for large
  lists without overflowing the stack.
   */

  /*
  Exercise 3.14

  Implement append in terms of either foldLeft or foldRight.
  */
  def append[A](ns1: List[A], ns2: List[A]): List[A] = foldRight(ns1, ns2)((lst2, lst1) => Cons(lst2, lst1))

  /*
  Exercise 3.15

  Hard: Write a function that concatenates a list of lists into a single list.
  Its runtime should be linear in the total length of all lists.
  Try to use functions we have already defined.
  */

  /*
  Exercise 3.16

  Write a function that transforms a list of integers by adding 1 to each element.
  (Reminder: this should be a pure function that returns a new List!)
  */
  def addOne(lst: List[Int]): List[Int] = foldRight(lst, List[Int]())((x, y) => Cons(x + 1, y))

  /*
  Exercise 3.17

  Write a function that turns each value in a List[Double] into a String.
  You can use the expression d.toString to convert some d: Double to a String.
  */
  def doubleToString(lst: List[Int]): List[String] = foldRight(lst, List[String]())((x, y) => Cons(x.toString, y))

  /*
  Exercise 3.18

  Write a function map that generalizes modifying each element in a list while maintaining the structure of the list.
  Here is its signature:[12]

  [Note] In the standard library, map and flatMap are methods of List.

  def map[A,B](as: List[A])(f: A => B): List[B]
  */
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, List[B]())((x, y) => Cons(f(x), y))

  /*
  Exercise 3.19"

  Write a function filter that removes elements from a list unless they satisfy a given predicate.
  Use it to remove all odd numbers from a List[Int].

  def filter[A](as: List[A])(f: A => Boolean): List[A]
  */
  def filterOld[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((x, y) => {
      f(x) match {
        case true => y
        case false => Cons(x, y)
      }
    })

  /*
  Exercise 3.20

  Write a function flatMap that works like map except that the function given will return a list instead of a single
  result, and that list should be inserted into the final resulting list.
  Here is its signature:

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]

  For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
  */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List[B]())((x, y) => append(f(x), y))

  /*
  Exercise 3.21

  Use flatMap to implement filter.
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
  */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((i) => f(i) match {
      case true => Cons(i, Nil)
      case false => Nil
    })

  /*
  Exercise 3.22

  Write a function that accepts two lists and constructs a new list by adding corresponding elements. For example,
  List(1,2,3) and List(4,5,6) become List(5,7,9).
  */
  //TODO: Looked at book answer, the better way to do this is to match on both a and b in one match statement
  def combine(a: List[Int], b: List[Int]): List[Int] = {
    a match {
      case Cons(x, y) => {
        b match {
          case Cons(x1, y1) => Cons(x + x1, combine(y, y1))
          case Nil => Nil
        }
      }
      case Nil => Nil
    }
  }

  /*
  Exercise 3.23

  Generalize the function you just wrote so that it’s not specific to integers or addition.
  Name your generalized function zipWith.
  */
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  /*
  Exercise 3.24

  Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence.
  For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
  You may have some difficulty finding a concise purely functional implementation that is also efficient. That’s okay.
  Implement the function however comes most naturally.
  We’ll return to this implementation in chapter 5 and hopefully improve on it.
  Note: Any two values x and y can be compared for equality in Scala using the expression x == y.

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean
  */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def loop[A](sup1: List[A], sub1: List[A], i: Int): Int = {
      if (i == length(sub))
        i
      else
        sup1 match {
          case Cons(h, t) => sub1 match {
            case Cons(h1, t1) => if (h == h1)
              if (t1 == Nil)
                1
              else
                loop(t, t1, i + 1)
            else
              loop(t, sub, 1)
            case Nil => i
          }
          case Nil => i
        }
    }
    loop(sup, sub, 1) == length(sub)
  }
}


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /*
  Exercise 3.25

  Write a function size that counts the number of nodes (leaves and branches) in a tree.

  */
  def treeSizeOld[A](t: Tree[A]): Int = {
    def loop[A](t1: Tree[A], i: Int): Int =
      t1 match {
        case Leaf(v) => i + 1
        case Branch(left, right) => loop(left, i) + loop(right, i) + 1
      }
    loop(t, 0)
  }

  /*
  Exercise 3.26

  Write a function maximum that returns the maximum element in a Tree[Int].
  (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
  */
  //TODO: Make this type generic
  def maximumOld(t: Tree[Int]): Int = {
    def loop(t1: Tree[Int], max: Int): Int = {
      t1 match {
        case Leaf(v: Int) => {
          if (v > max) {
            v
          } else {
            max
          }
        }
        case Branch(left, right) =>
          val lv = loop(left, max)
          val rv = loop(right, max)
          if (lv > rv)
            lv
          else
            rv
      }
    }
    loop(t, -100)
  }

  /*
  Exercise 3.27

  Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  */
  def depthOld[A](t: Tree[A]): Int = {
    def loop[A](t1: Tree[A], currMax: Int, currDepth: Int): Int = {
      t1 match {
        case Leaf(v: Int) =>
          if ((currDepth + 1 < currMax)) {
            currMax
          } else {
            currDepth + 1
          }
        case Branch(left, right) =>
          val lDepth = loop(left, currMax, currDepth + 1)
          val rDepth = loop(right, currMax, currDepth + 1)
          if (lDepth > rDepth) lDepth else rDepth

      }
    }
    loop(t, 0, 0)
  }

  /*
  Exercise 3.28

  Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function.
  */
  def mapOld[A](t: Tree[A], f: (A => A)): Tree[A] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(map(left, f), map(right, f))
    }
  }

  /*
  Exercise 3.29

  Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
  Reimplement them in terms of this more general function.
  Can you draw an analogy between this fold function and the left and right folds for List?
  */
  def fold[A](t: Tree[A])( f: ((A, Int) => Int ), f2: ((Int) => Int ), init: Int): Int = {
    t match {
      case Leaf(v) => f(v,init)
      case Branch(left, right) => fold(right)(f, f2, fold(left)(f, f2, f2(init)))
    }
  }

  def treeSize[A](t: Tree[A]): Int = {
    fold(t)((x: A,y: Int) => y + 1, (v: Int) => v + 1, 0)
  }

  def maximum(t: Tree[Int]): Int = {
    fold(t)((x: Int,y: Int) => if( y > x){y} else x, (v: Int) => v, 0)
  }

  def depth[A](t: Tree[A]): Int = {
    fold(t)((x: A,y: Int) => y, (v: Int) => v + 1, 0)
  }

  //TODO:

  def map[A](t: Tree[A], f: (A => A)): Tree[A] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(map(left, f), map(right, f))
    }
  }


}







