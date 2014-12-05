package fpis.parallelism

sealed trait Par[+A]

object Par {
  def unit[A](a: => A): Par[A] = ???
  def get[A](pa: Par[A]): A = ???

  // Ex 7.1
  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = ???
}

object Exs {
  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] = if (ints.size <= 1) {
    Par.unit(ints.headOption getOrElse 0)
  } else {
    val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(sum(l), sum(r))(_ + _)
  }
}
