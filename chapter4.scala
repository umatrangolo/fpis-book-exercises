package fpis.errors

import scala.{ Option => _, Either => _ }

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

// Ex 4.1 Implement all the above functions
case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None
  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  override def getOrElse[B >: Nothing](default: => B): B = default
  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  override def filter(f: Nothing => Boolean): Option[Nothing] = None
}

case class Some[+A](private val elem: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(elem))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(elem)
  override def getOrElse[B >: A](default: => B): B = elem
  override def orElse[B >: A](ob: => Option[B]): Option[B] = this
  override def filter(f: A => Boolean): Option[A] = if (f(elem)) this else None
}

object Chapter4 {

  // Ex 4.2 Implement the variance function in terms of flatMap. If the
  // mean of a sequence is m, the variance is the mean of math.pow(x - m, 2)
  // for each element x in the sequence.
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.size)
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap { m => mean(xs.map { x => math.pow(x - m, 2) }) }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _.map(f)

  def Try[A](a: => A): Option[A] = try Some(a) catch { case e: Exception => None }

  // Ex 4.3
  // Write a generic function map2 that combines two Option(s)
  // values using a binary function. If either Option value is None,
  // then the return value is too.
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    ra <- a
    rb <- b
  } yield {
    f(ra, rb)
  }

  // Ex 4.4
  // Write a function sequence that combines a list of Options into one
  // Option containing a list of all the Some values in the original
  // list. If the original list contains None even once, the result of
  // the function should be None; otherwise the result should be Some
  // with a list of all the values.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap { hh => sequence(t).map { hh :: _ } }
  }

  // Ex 4.5
  // Implement this function. It’s straightforward to do using map and
  // sequence, but try for a more efficient implementation that only
  // looks at the list once. In fact, implement sequence in terms of
  // traverse.
  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] = xs match {
    case Nil => Some(Nil)
    case head :: tail => f(head).flatMap { res => traverse(tail)(f).map { res :: _ } }
  }
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a) { x => x }
}


sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing]{
  override def map[B](f: Nothing => B): Either[E, B] = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
  override def orElse[EE >: E,B >: Nothing](alternative: => Either[EE, B]): Either[EE, B] = alternative
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
  override def orElse[EE >: Nothing,B >: A](b: => Either[EE, B]): Either[EE, B] = this
  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.flatMap { r => Right(f(value, r)) }
}

object Either {
  def Try[A](a: => A): Either[Exception, A] = try Right(a) catch { case e: Exception => Left(e) }

  // Ex 4.8
  // Implement sequence and traverse for Either. These should return the
  // first error that’s encountered, if there is one.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es) { e => e }

  def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] = xs match {
    case Nil => Right(List.empty[B])
    case head :: tail => f(head).flatMap { r => traverse(tail)(f).map { r :: _ } }
  }

}
