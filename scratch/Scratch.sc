import math.abs

object Scratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def product(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, 1, (x, y) => x * y)(a, b)        //> product: (f: Int => Int)(a: Int, b: Int)Int

  def mapReduce(f: Int => Int, u: Int, s: (Int, Int) => Int)(a: Int, b: Int): Int =
    if (a > b) u
    else s(f(a), mapReduce(f, u, s)(a + 1, b))    //> mapReduce: (f: Int => Int, u: Int, s: (Int, Int) => Int)(a: Int, b: Int)Int

  product(identity)(3, 5)                         //> res0: Int = 60

  def fact(i: Int) = product(identity)(1, i)      //> fact: (i: Int)Int
  fact(5)                                         //> res1: Int = 120

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, 0, (x, y) => x + y)(a, b)        //> sum: (f: Int => Int)(a: Int, b: Int)Int

  sum(x => x * x)(1, 3)                           //> res2: Int = 14


  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def isCloseEnough(x: Double, y: Double): Boolean = abs((x - y) / x) / x < 0.0001
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  fixedPoint(x => 1 + x/2)(1)                     //> res3: Double = 1.999755859375
  
  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
  
  def sqrt(x: Double) =
    fixedPoint(averageDamp(y => x / y))(1.0)      //> sqrt: (x: Double)Double

  sqrt(2)                                         //> res4: Double = 1.4142135623746899
}