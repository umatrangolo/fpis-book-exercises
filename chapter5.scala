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

  // Ex 5.13
  // Use unfold to implement map, take, takeWhile, zipWith (as in chapter
  //   3), and zipAll. The zipAll function should continue the traversal as
  // long as either stream has more elements—it uses Option to indicate
  // whether each stream has been exhausted.
  def map2[B](f: A => B): Stream[B]
  def take2(n: Int): Stream[A]
  def takeWhile3(p: A => Boolean): Stream[A]
  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C]
  def zipAll[B](other: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold(this, other) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), Stream.empty[B]))
    case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (Stream.empty[A], t2())))
    case _ => None
  }

  // Ex 5.14
  // Implement startsWith using functions you’ve written. It should check
  // if one Stream is a prefix of another. For instance, Stream(1,2,3)
  // startsWith Stream(1,2) would be true.
  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile { !_._2.isEmpty }.forAll {
    case (h,h2) => h == h2
  }
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
  override def map2[B](f: Nothing => B): Stream[B] = Stream.empty[B]
  override def take2(n: Int) = Stream.empty[Nothing]
  override def takeWhile3(p: Nothing => Boolean) = Stream.empty[Nothing]
  override def zipWith[B, C](s2: Stream[B])(f: (Nothing, B) => C): Stream[C] = Stream.empty[C]
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
  override def map2[B](f: A => B): Stream[B] = Stream.unfold(this: Stream[A]) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }
  override def take2(n: Int): Stream[A] = Stream.unfold((this: Stream[A], n)) {
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _ => None
  }
  override def takeWhile3(p: A => Boolean): Stream[A] = Stream.unfold((this: Stream[A])) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }
  override def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this: Stream[A], other: Stream[B])) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }
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

  // Ex 5.9
  // Write a function that generates an infinite stream of integers,
  // starting from n, then n + 1, n + 2, and so on.
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // Ex 5.10
  // Write a function fibs that generates the infinite stream of
  // Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  def fib(): Stream[Int] = {
    def _fib(a: Int, b: Int): Stream[Int] = Stream.cons(a, _fib(b, a + b))
    Stream.cons(0, _fib(0, 1))
  }

  // Ex 5.11
  // Write a more general stream-building function called unfold. It
  // takes an initial state, and a function for producing both the next
  // state and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => Stream.cons(h, unfold(s)(f))
    case None => Stream.empty
  }

  // unfold() is an example of a co-recursive function. A co-recursive
  // function works by continually producing new values until they
  // stop. A recursive function instead consumes data and stops when it
  // consumed it all.

  // Ex 5.12
  def fib2(): Stream[Int] = unfold((0, 1)) { z => Some((z._1, (z._2, z._1 + z._2))) }
  def from2(n: Int): Stream[Int] = unfold(n) { z => Some((z + 1, z + 1)) }
  def constant3[A](a: A): Stream[A] = unfold(a) { z => Some(a, a) }
  val ones: Stream[Int] = unfold(1) { z => Some(1, 1) }
}
