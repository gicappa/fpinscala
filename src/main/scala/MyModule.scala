import scala.annotation.tailrec

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    s"The absolute value of %d is %d" format(x, abs(x))
  }

  private def formatFactorial(x: Int) = {
    s"The factorial of %d is %d" format(x, factorial(x))
  }

  private def formatFib(x: Int) = {
    s"The fibonacci number of %d is %d" format(x, fib(x))
  }

  def fib(x: Int): Int = {
//    @tailrec
    def go(x: Int, acc: Int): Int =
      if (x < 2) acc + x
      else go(x - 1, acc) + go(x - 2, acc)

    go(x, 0)
  }

  def factorial(x: Int): Int = {
    @tailrec
    def go(x: Int, acc: Int): Int =
      if (x <= 0) acc
      else go(x - 1, acc * x)

    go(x, 1)
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

  println(formatFactorial(7))
  println(formatFib(11))
}