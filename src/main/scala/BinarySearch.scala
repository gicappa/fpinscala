import scala.annotation.tailrec

object BinarySearch {
  def binarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }

    go(0, 0, as.length - 1)
  }


  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {

    def go(x: Int): Boolean = {
      if (x == 0) true
      else
        if (gt(as(x), as(x - 1))) go(x - 1)
        else false
    }

    go(as.length - 1)
  }

  def main(args: Array[String]): Unit = {
    printResult(Array(75))
    printResult(Array(75, 80, 92, 76, 54))
    printResult(Array(54, 75, 76, 80, 92))
  }

  private def printResult(ints: Array[Int]) = {
    println("array %s / isSorted %s".
      format(arrayToString(ints), isSorted[Int](ints, (x: Int, y: Int) => x > y)))
  }

  private def arrayToString(ints: Array[Int]) = {
    ints.deep.mkString(", ")
  }
}
