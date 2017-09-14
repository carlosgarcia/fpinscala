sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // ex 3.1
  def ex1(): Unit = {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    println("x is " + x) // expected 3
  }

  // ex 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }
  }

  // ex 3.3
  def setHead[A](l: List[A], head:A): List[A] = {
    l match {
      case Nil => sys.error("setHead of empty list")
      case Cons(_, xs) => Cons(head, xs)
    }
  }

  // ex 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  // ex 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
  }

  // ex 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def main(args: Array[String]): Unit = {
    ex1()

    // println(tail(List()))  // expected error
    println(tail(List(1)))
    println(tail(List(1,2)))
    println(tail(List(1,2,3)))

    //println(setHead(List(), 8))  // expected error
    println(setHead(List[Int](1), 8))
    println(setHead(List[Int](1,2), 8))
    println(setHead(List[Int](1,2,3), "aa"))

    println("drop " + drop(List(1), 1))
    println("drop " + drop(List(1,2), 1))
    println("drop " + drop(List(1,2,3), 2))
    println("drop " + drop(List(1,2,3), 5))

    val isEven:Function[Int, Boolean] = x => (x%2) == 1
    println("dropWhile " + dropWhile(List(1,3,7,2), isEven))
    println("dropWhile " + dropWhile(List(2,3), isEven))
    println("dropWhile " + dropWhile(List(), isEven))
    println("dropWhile " + dropWhile(List(1), isEven))
    println("dropWhile " + dropWhile(List(1,2,3), isEven))


    // 3.8
    println("---- 3.8 ----")
    println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  }
}