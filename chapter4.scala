import scala.{ Option => _ }

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


}
