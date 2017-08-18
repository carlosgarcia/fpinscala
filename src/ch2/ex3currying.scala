package ch2

/**
  * Created by carlos on 17/8/17.
  */
object ex3currying {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a:A) => (b:B) =>f(a, b) // same as (a:A) => ((b:B) =>f(a, b)) , but => associates to the right by default
  }


  def main(args: Array[String]): Unit = {
    def f : Function2[Int, Int, Int] = (a:Int, b:Int) => a+b
    println(f(2,3))
    def curried = curry(f)
    println((curried(2))(3))


  }
}
