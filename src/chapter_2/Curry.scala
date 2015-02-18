package chapter_2

/*
Exercise 2.3

Let’s look at another example, currying,[9] which converts a function f of two arguments into a function of one argument that partially applies f.
Here again there’s only one implementation that compiles. Write this implementation. 9 This is named after the mathematician Haskell Curry,
who discovered the principle.
It was independently discovered earlier by Moses Schoenfinkel, but Schoenfinkelization didn’t catch on.

def curry[A,B,C](f: (A, B) => C): A => (B => C)
 */

object Curry {
  def main(args: Array[String]): Unit = {
		  val func2 = curry((n1: Int, n2: Int) => n1 + n2)
		  val func3 = func2(3)
		  val result = func3(4)
		  assert(result == 7)
		  
  }

  /*
   * EXERCISE 3: Let's look at another example, currying, which converts a function f of two arguments into a function of one 
   * argument that partially applies f. 
   * Here again there's only one implementation that compiles. Write this implementation.
   * 
   *  def curry[A,B,C](f: (A, B) => C): A => (B => C)
   *  
   */

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (f(a: A, _: B))

  /* Andrew's alternate solution */
  def curry2[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)
 
}