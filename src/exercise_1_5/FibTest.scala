package exercise_1_5

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