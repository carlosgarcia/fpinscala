package ch2

/**
  * Created by carlos on 17/8/17.
  */
object ex4uncurrying {
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a:A, b:B) => f(a)(b)
  }


  def main(args: Array[String]): Unit = {
    def f : Function[Int, Function[Int, Int]] = (a:Int) => (b:Int) => a+b
    println(f(5)(3))
    def uncurried = uncurry(f)
    println(uncurried(5, 3))
  }
}
