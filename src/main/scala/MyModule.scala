import scala.annotation.tailrec

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n


  private def formatResult(msg: String, x: Int, f: Int => Int) = {
    s"The %s of %d is %d" format(msg, x, f(x))
  }

  private def formatFactorial(x: Int) = {
    s"The factorial of %d is %d" format(x, factorial(x))
  }

  private def formatFib(x: Int) = {
    s"The fibonacci number of %d is %d" format(x, fib(x))
  }

  def fib(x: Int): Int = {
    @tailrec
    def go(i: Int, fib: Int, last: Int): Int =
      if (i < 2) fib
      else go(i - 1, fib + last, fib)

    go(x, 1, 0)
  }

  def factorial(x: Int): Int = {
    @tailrec
    def go(x: Int, acc: Int): Int =
      if (x <= 0) acc
      else go(x - 1, acc * x)

    go(x, 1)
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibonacci number", 18, fib))
  }

}