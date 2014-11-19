package fpis.laziness

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

  // Ex 5.5
  // Use foldRight to implement takeWhile.
  def takeWhile2(p: A => Boolean): Stream[A]

  // Ex 5.6
  // Implement headOption using foldRight.
  def headOption2: Option[A]

  // Ex 5.7
  // Implement map, filter, append, and flatMap using foldRight. The
  // append method should be non-strict in its argument.
  def map[B](f: A => B): Stream[B]
  def filter(p: A => Boolean): Stream[A]
  def flatMap[B](f: A => Stream[B]): Stream[B]
  def append[B >: A](other: => Stream[B]): Stream[B]
}

case object Empty extends Stream[Nothing] {
  override def headOption = None
  override def toList = Nil
  override def take(n: Int) = Stream.empty
  override def drop(n: Int) = Stream.empty
  override def takeWhile(p: Nothing => Boolean) = Stream.empty
  override def forAll(p: Nothing => Boolean): Boolean = false
  override def takeWhile2(p: Nothing => Boolean) = Stream.empty
  override def headOption2: Option[Nothing] = None
  override def map[B](f: Nothing => B) = Stream.empty[B]
  override def filter(p: Nothing => Boolean) = Stream.empty[Nothing]
  override def flatMap[B](f: Nothing => Stream[B]) = Stream.empty[B]
  override def append[B >: Nothing](other: => Stream[B]) = other
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
  override def forAll(p: A => Boolean): Boolean = foldRight(true) { (a, b) => p(a) && b }
  override def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty[A] }
  override def headOption2: Option[A] = foldRight(None: Option[A]) { (h, _) => Some(h) }
  override def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B]) { (h, t) => Stream.cons(f(h), t) }
  override def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (h, t) => if (p(h)) Stream.cons(h, t) else t }
  override def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B]) { (h, t) => f(h).append(t) }
  override def append[B >: A](other: => Stream[B]): Stream[B] = foldRight(other) { (h, t) => Stream.cons(h, t) }
}

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // Ex 5.8
  // Write a function which returns an infinite Stream of a given value.
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

}
