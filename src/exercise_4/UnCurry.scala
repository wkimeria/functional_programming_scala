package exercise_4

object UnCurry {

  def main(args: Array[String]): Unit = {

  }

  /*
   * EXERCISE 4: Implement uncurry, which reverses the transformation of curry. 
   * Note that since => associates to the right, A => (B => C) 
   * can be written as A => B => C. 
   * 
   * def uncurry[A,B,C](f: A => B => C): (A, B) => C
   * 
   */

  //http://stackoverflow.com/questions/3456864/how-do-i-get-a-b-c-from-a-b-c-in-scala
  //val curry = (i: Int) => ((s: String) => i*s.length)
  //val uncurry = (i: Int, s: String) => curry(i)(s)
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

}