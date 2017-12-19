sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(l: List[Int]): Int =
    foldRight(l, 0)(_ + _)

  def product(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def myMatch(xs: List[Int]): Any = xs match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def tail[A](ds: List[A]): List[A] = ds match {
    case Nil => List()
    case Cons(_, xs) => xs
  }

  def drop[A](ds: List[A], n: Int): List[A] = ds match {
    case Nil => List()
    case Cons(_, xs) =>
      if (n == 1) xs
      else drop(xs, n - 1);
  }

  def dropWhile[A](ds: List[A])(f: A => Boolean): List[A] = ds match {
    case Nil => List()
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => ds
  }

  def setHead[A](ds: List[A], h: A): List[A] = ds match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def main(args: Array[String]): Unit = {
    println(product(List(1, 0, 3, 4, 5)))
    //    println(drop(List(1, 2, 3, 4, 5), 3))
    //    println(dropWhile(List(1, 2, 3, 4, 5))(_ < 3))
    //    println(setHead(List(1, 2, 3, 4, 5), 9))
    //    println(init(List(1, 2, 3, 4, 5)))
  }
}


