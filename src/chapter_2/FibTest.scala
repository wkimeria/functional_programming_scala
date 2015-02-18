package chapter_2
/*
Exercise 2.1

Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s). The first two Fibonacci numbers are 0 and 1.
The nth number is always the sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5.
Your definition should use a local tail-recursive function. def fib(n: Int): Int
 */

object FibTest {
  def main(args: Array[String]): Unit = {
    println(fib(5))
  }

  /* Get the nth Fibonacci number */
  def fib(pos: Int): Int = {
    @annotation.tailrec
    var counter = 2
    def go(left: Int, right: Int, pos: Int): Int = {
      if (pos == 0) 0
      if (pos == 1) 1
      val res = left + right
      if (counter == pos) res
      else {
        counter = counter + 1
        go(right, (left + right), pos)
      }
    }
    go(0, 1, pos)
  }
}