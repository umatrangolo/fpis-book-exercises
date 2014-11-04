import scala.annotation.tailrec

object chapter2 {

  // Ex 2.1
  // 0 1 1 2 3 5 8 13 ...
  // Should be tail recursive
  @tailrec
  def fib(n: Long, i: Long = 0, a: Long = 0, b: Long = 1): Long = n match {
    case 0 => 0
    case 1 => 1
    case _ => if (i == n) b else fib(n, i + 1, b, a + b)
  }

  // Ex 2.2
  // Implement isSorted, which checks whether an Array[A] is sorted
  // according to a given comparison function
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def scan(i: Int): Boolean = if (i == as.size - 1) true else if (ordered(as(i), as(i + 1))) scan(i + 1) else false
    if (as.isEmpty) true else scan(0)
  }

  // Ex 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  // Ex 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
}
