package week4

object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
  }
  
  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty: Boolean = false
  }
  
  class Nil[T] extends List[T] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  }
  
  def nth[T](n: Int, l: List[T]): T = {
    if (n < 0 || l.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) l.head
    else nth(n - 1, l.tail)
  }                                               //> nth: [T](n: Int, l: week4.worksheet.List[T])T

  val l = new Cons(1, new Cons(3, new Cons(5, new Nil)))
                                                  //> l  : week4.worksheet.Cons[Int] = week4.worksheet$$anonfun$main$1$Cons$1@b50d
                                                  //| af
  nth(0, l)                                       //> res0: Int = 1
  nth(1, l)                                       //> res1: Int = 3
  nth(2, l)                                       //> res2: Int = 5
  nth(-1, l)                                      //> java.lang.IndexOutOfBoundsException
                                                  //| 	at week4.worksheet$$anonfun$main$1.nth$1(week4.worksheet.scala:23)
                                                  //| 	at week4.worksheet$$anonfun$main$1.apply$mcV$sp(week4.worksheet.scala:32
                                                  //| )
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week4.worksheet$.main(week4.worksheet.scala:3)
                                                  //| 	at week4.worksheet.main(week4.worksheet.scala)
}