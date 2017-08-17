package ch2

/**
  * Created by carlos on 17/8/17.
  */
object ex1Fibonacci {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, previous: Int, current: Int): Int =
      if (n == 0) previous
      else go(n - 1, current, current + previous)
    go(n-1, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(fib(4))
    println(fib(5))
    println(fib(6))
    println(fib(7))
  }
}
