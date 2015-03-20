package chapter_2
/*
Exercise 2.1

Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s). The first two Fibonacci numbers are 0 and 1.
The nth number is always the sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5.
Your definition should use a local tail-recursive function. def fib(n: Int): Int
 */

object FibTest {
  def main(args: Array[String]): Unit = {
    println(fib(6))
  }

  /* Get the nth Fibonacci number */
  def fib(pos: BigInt): BigInt = {
    @annotation.tailrec
    def loop(left: BigInt, right: BigInt, pos: BigInt, counter: BigInt): BigInt = {
      if (pos < 2) 0
      if (counter == pos) left + right
      else {
        loop(right, (left + right), pos, counter + 1)
      }
    }
    loop(0, 1, pos, 2)
  }
}