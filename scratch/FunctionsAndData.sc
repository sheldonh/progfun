object FunctionsAndData {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val x = new Rational(1, 3)                      //> x  : Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : Rational = 3/2
  
  -x                                              //> res0: Rational = 1/-3
  
  x + y                                           //> res1: Rational = 22/21
  
  x - y - z                                       //> res2: Rational = -79/42
  y + y                                           //> res3: Rational = 10/7
  
  x < y                                           //> res4: Boolean = true
  x max y                                         //> res5: Rational = 5/7
}


class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")
  
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  val g = gcd(x, y)

  def numer = x/g
  def denom = y/g
  
  override def toString = numer + "/" + denom
  
  def <(that: Rational) = numer * that.denom < that.numer * denom
  
  def max(that: Rational) = if (this < that) that else this
  
  def +(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  
  def -(that: Rational) = this + -that
  
  def unary_- = new Rational(-numer, denom)

}