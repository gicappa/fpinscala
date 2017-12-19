object PartialApplication {
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))


  def main(args: Array[String]): Unit = {
    val fun = partial1[Int, String, Boolean](1, (n, nStr) => n.toString == nStr)

    println(fun("1"))
    println(fun("4"))
  }


}
