import scala.{ Stream => _ }

sealed trait Stream[+A] {
  def headOption: Option[A]

  // Ex 5.1
  // Write a function to convert a Stream to a List, which will force its
  // evaluation and let you look at it in the REPL.
  def toList: List[A]
}
case object Empty extends Stream[Nothing] {
  override def headOption = None
  override def toList = Nil
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def headOption: Option[A] = Some(h())
  override def toList: List[A] = h() :: t().toList
}

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
