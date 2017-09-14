package ch4

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(a) => Left(a)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => f(a)
      case Left(a) => Left(a)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => b
      case Right(e) => Right(e)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  def map22[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

    //this.flatMap(aa => b.map(bb => f(aa, bb)))
  }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil)
      case h::t => f(h).flatMap(hh => traverse(t)(f).map(tt => hh::tt))
    }
  }

  def traverse2[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil)
      case h::t => {
        for {
          hh <- f(h)
          tt <- traverse(t)(f)
        } yield hh::tt
      }
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(identity)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}

object EitherRunner {

  def main(args: Array[String]): Unit = {

    //println(Some(2).map(x => x+3))
    //println(None.map(y => y))

    println("--- sequence ---")
    println(Either.sequence(List(Right(1), Right(2), Right(3))))
    println(Either.sequence(List(Right(1), Right(2), Left("error"))))
    println(Either.sequence(Nil))
    println(Either.sequence(List()))

    println("--- traverse ---")
    println(Either.traverse(List("123", "434"))(x => Either.Try(x.toInt)))
    println(Either.traverse(List("123", "aaa"))(x => Either.Try(x.toInt)))

    println("--- traverse2 ---")
    println(Either.traverse2(List("123", "434"))(x => Either.Try(x.toInt)))
    println(Either.traverse2(List("123", "aaa"))(x => Either.Try(x.toInt)))
  }
}