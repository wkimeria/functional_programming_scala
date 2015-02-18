package chapter_2

/*

Exercise 2.2

Implement isSorted, which checks whether an Array[A] is sorted according
to a given comparison function:

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
 */

object Sorted {
  def main(args: Array[String]): Unit = {
    val sortedAsc1 = isSorted(Array(1, 2, 3, 4, 5, 6), (n1: Int, n2: Int) => n1 <= n2)
    assert(sortedAsc1 == true)

    val sortedAsc2 = isSorted(Array(1, 1, 1, 4, 5, 6), (n1: Int, n2: Int) => n1 <= n2)
    assert(sortedAsc2 == true)

    val sortedDesc = isSorted(Array(6, 5, 4, 3, 2, 1), (n1: Int, n2: Int) => n1 >= n2)
    assert(sortedDesc == true)

    val sortedDesc2 = isSorted(Array(6, 5, 4, 3, 3, 3, 2, 1), (n1: Int, n2: Int) => n1 >= n2)
    assert(sortedDesc2 == true)

    val upDown = isSorted(Array(1, 2, 3, 4, 3, 2, 1), (n1: Int, n2: Int) => n1 >= n2)
    assert(upDown == false)
    
    val sortedAscStr = isSorted(Array("a", "b", "c"), (n1: String, n2: String) => n1 <= n2)
    assert(sortedAscStr == true)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def loop(n1: Int, n2: Int): Boolean =
      if (n2 >= as.length) true
      else if (gt(as(n1), as(n2)) == false) false
      else loop(n1 + 1, n2 + 1)
    loop(0, 1)
  }

}