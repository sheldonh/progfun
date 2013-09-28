object FunctionsAndData {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val x = new Rational(1, 3)                      //> x  : Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : Rational = 3/2
  
  x.neg                                           //> res0: Rational = -1/3
  
  x.add(y)                                        //> res1: Rational = 22/21
  
  x.sub(y).sub(z)                                 //> res2: Rational = -79/42
}

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
  
  override def toString = x + "/" + y
  
  def add(other: Rational) =
    new Rational(
      numer * other.denom + other.numer * denom,
      denom * other.denom)
  
  def sub(other: Rational) = add(other.neg)
  
  def neg = new Rational(-numer, denom)

}