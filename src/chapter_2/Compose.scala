package chapter_2

/*
Exercise 2.5

Implement the higher-order function that composes two functions.

def compose[A,B,C](f: B => C, g: A => B): A => C
 */

/**
 * Found this link helpful
 * https://twitter.github.io/scala_school/pattern-matching-and-functional-composition.html#composition
 */
object Compose {
  def main(args: Array[String]): Unit = {
    def f(s: String) = "f(" + s + ")"
    def g(s: String) = "g(" + s + ")"
    val composed = compose(f,g)
    println(composed("Hello"))
  }

  /*EXERCISE 5: Implement the higher-order function that composes two functions.*/
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a: A))
  }
}