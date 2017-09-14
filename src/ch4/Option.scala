package ch4

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter


case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  def getOrElse[B>:A](default: => B): B = {
    this match {
      case Some(a) => a
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa,bb)))
  }

  def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = {
    a.flatMap(aa => b.flatMap(bb => (c.map(cc => f(aa,bb,cc)))))
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]):Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h::t => sequence(t).flatMap(tt => h.map(hh => hh::tt))
    }
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(identity)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h::t => f(h).flatMap(hh => traverse(t)(f).map(tt => hh::tt))
    }
  }


}


object OptionRunner {

  def Try[A](a: => A):Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  def main(args: Array[String]): Unit = {

    //println(Some(2).map(x => x+3))
    //println(None.map(y => y))

    println("--- sequence ---")
    println(Some(0).sequence(List(Some(1), Some(2), Some(3))))
    println(Some(0).sequence(List(Some(1), Some(2), None)))
    println(Some(0).sequence(Nil))
    println(Some(0).sequence(List()))

    println("--- sequence2 ---")
    println(Some(0).sequence2(List(Some(1), Some(2), Some(3))))
    println(Some(0).sequence2(List(Some(1), Some(2), None)))
    println(Some(0).sequence2(Nil))
    println(Some(0).sequence2(List()))

    println("--- traverse ---")
    println(Some(0).traverse(List("123", "434"))(x => Try(x.toInt)))
    println(Some(0).traverse(List("123", "aaa"))(x => Try(x.toInt)))

  }
}