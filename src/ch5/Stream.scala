package ch5

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case Empty => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h,t) if n == 1 => cons(h(), Empty)
      case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
      case _ => Empty
    }
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h,t) if n > 0 => t().drop(n - 1)
      case Cons(h,t) if n == 0 => t()
      case _ => Empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  def forAll(f: A => Boolean): Boolean = {
    foldRight(true)((a, b) => f(a) && b)
  }

  def takeWhile_fold(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h,t)
      else empty
    )
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((h,_) => Some(h))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B):Stream[B] = {
    foldRight(Empty: Stream[B])((h,t) => cons(f(h), t))
  }

  def filter(f: A => Boolean):Stream[A] = {
    foldRight(Empty: Stream[A])((h,t) => {
      if (f(h)) cons(h, t)
      else t
    })
  }

  def append[B>:A](s: => Stream[B]):Stream[B] = {
    foldRight(s)((h,t) => cons(h,t))
  }

  def flatMap[B](f: A => Stream[B]):Stream[B] = {
    foldRight(empty[B])((h,t) => f(h).append(t))
  }

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList:List[A] = {
    this match {
      case Empty => List()
      case Cons(h, t) => h()::t().toList
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def fibs: Stream[Int] = {
    def loop(a:Int, b:Int):Stream[Int] = {
      println("loop of " + a + ", b " + b)
      cons(a, loop(b, a+b))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }

  def fibs_unfold: Stream[Int] = {
    unfold((0,1)){case (a,b) => Some(a+b, (b,a+b))}
  }

  def from_unfold(n: Int): Stream[Int] = {
    unfold(n)(x => Some(x, x+1))
  }

  def constant_unfold(n: Int): Stream[Int] = {
    unfold(n)(x => Some(x, x))
  }

  def ones_unfold: Stream[Int] = {
    unfold(1)(_ => Some(1, 1))
  }

}

object StreamRunner {

  def main(args: Array[String]): Unit = {

    //println(Some(2).map(x => x+3))
    //println(None.map(y => y))

    println("--- takeWhile ---")
    val a:Stream[Int] = cons(2, cons(6, cons(5, cons(10, Empty))))
    println(a.takeWhile_fold((x) => x%2 == 0).toList)


    println("--- flatMap ---")
    println(a.flatMap((x:Int) => {
      if (x%2 == 0) cons("EVEN", Empty)
      else cons("ODD", Empty)
    }).toList)

    println("--- constant ---")
    println(constant(3).exists(x => x%2 != 0))

    println("--- from ---")
    println(from(3).takeWhile(x => x < 10).toList)

    println("--- fibs ---")
    println(fibs.takeWhile(x => x < 20).toList)
  }
}