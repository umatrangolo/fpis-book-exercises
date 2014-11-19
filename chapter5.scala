sealed trait Stream[+A] {
  def headOption: Option[A]

  // Ex 5.1
  // Write a function to convert a Stream to a List, which will force its
  // evaluation and let you look at it in the REPL.
  def toList: List[A]

  // Ex 5.2
  // Write the function take(n) for returning the first n elements of a
  // Stream, and drop(n) for skipping the first n elements of a Stream.
  def take(n: Int): Stream[A]
  def drop(n: Int): Stream[A]

  // Ex 5.3
  // Write the function takeWhile for returning all starting elements of
  // a Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A]

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false) { (a, b) => p(a) || b }

  // Ex 5.4
  // Implement forAll, which checks that all elements in the Stream match
  // a given predicate. Your implementation should terminate the
  // traversal as soon as it encounters a nonmatching value.
  def forAll(p: A => Boolean): Boolean
}

case object Empty extends Stream[Nothing] {
  override def headOption = None
  override def toList = Nil
  override def take(n: Int) = Stream.empty
  override def drop(n: Int) = Stream.empty
  override def takeWhile(p: Nothing => Boolean) = Stream.empty
  override def forAll() = false
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def headOption: Option[A] = Some(h())
  override def toList: List[A] = h() :: t().toList
  override def take(n: Int): Stream[A] = if (n == 0) Stream.empty else this match {
    case Cons(h, t) if n == 1 => Cons(h, () => Stream.empty)
    case Cons(h, t) => Cons(h, () => t().take(n - 1))
  }
  override def drop(n: Int): Stream[A] = if (n == 0) this else t().drop(n - 1)
  override def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case Cons(h, t) => t().takeWhile(p)
    case _ => Stream.empty
  }
  def forAll(p: A => Boolean): Boolean = foldRight(true) { (a, b) => p(a) && b }
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
