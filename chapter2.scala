import scala.annotation.tailrec

object chapter2 {

  // 0 1 1 2 3 5 8 13 ...
  // Should be tail recursive
  @tailrec
  def fib(n: Long, i: Long = 0, a: Long = 0, b: Long = 1): Long = n match {
    case 0 => 0
    case 1 => 1
    case _ => if (i == n) b else fib(n, i + 1, b, a + b)
  }
}
