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
