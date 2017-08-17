package ch2

import ch2.ex1Fibonacci.fib

/**
  * Created by carlos on 17/8/17.
  */
object ex2isSorted {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(i: Int): Boolean = {
      if (i >= as.length) true
      else if(ordered(as(i-1), as(i))) loop(i+1)
      else false
    }

    loop(1)
  }


  def main(args: Array[String]): Unit = {
    println(isSorted(Array(), (_: Int,_: Int) => false)) // true
    println(isSorted(Array(1), (_: Int,_: Int) => false)) // true
    println(isSorted(Array(1,2,3), (a: Int, b: Int) => a < b)) // true
    println(isSorted(Array(3,2,3), (a: Int, b: Int) => a <= b)) // false
    println(isSorted(Array(1,2,2,3), (a: Int, b: Int) => a <= b)) // true
    println(isSorted(Array(4,2,3), (a: Int, b: Int) => a%2 <= b%2)) // true
    println(isSorted(Array(4,1,2,3), (a: Int, b: Int) => a%2 < b%2)) // false
  }
}
