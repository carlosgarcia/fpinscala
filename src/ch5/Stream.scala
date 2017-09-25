package ch5

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
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
      else empty[A]
    )
  }

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

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
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}
