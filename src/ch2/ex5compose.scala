package ch2

/**
  * Created by carlos on 17/8/17.
  */
object ex5compose {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a:A) => f(g(a))
  }


  def main(args: Array[String]): Unit = {
    def first : Function[Array[Int], Int] = (a:Array[Int]) => a(0)
    def isEven : Function[Int, Boolean] = (a:Int) => (a%2) == 0

    val ints = Array(2, 3, 4)
    def composed = compose(isEven, first)
    println(isEven(first(ints)))
    println(composed(ints))

    println("--------")
    println(isEven(first(Array(3, 4))))
    println(composed(Array(3, 4)))

  }
}
