package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    val post = Vector(1)

    def triangle(r: Int): Vector[Int] =
      if (r == 0) post
      else {
        val t = triangle(r - 1)
        post ++ t.iterator.sliding(2, 1).withPartial(false).map(p => p(0) + p(1)).toVector ++ post
      }

    triangle(r)(c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countOpen(open: Int, chars: List[Char]): Int =
      if (open < 0) -1
      else if (chars.isEmpty) open
      else
        countOpen(chars.head match {
          case '(' => open + 1
          case ')' => open - 1
          case _ => open
        }, chars.tail)

    countOpen(0, chars) == 0
  }

  /**
   * Exercise 3
   */

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

}
