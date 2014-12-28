package fpis.monoids

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }

  // Ex 10.1
  // Give Monoid instances for integer addition and multiplication as
  // well as the Boolean operators.
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 | a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 & a2
    override def zero: Boolean = true
  }

  // Ex 10.2
  // Give a Monoid instance for combining Option values.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    def zero: Option[A] = None
  }

  // Ex 10.3
  // A function having the same argument and return type is sometimes
  // called an endofunction. Write a monoid for endofunctions.

  // En endo function is a function that has domain == co-domain, that
  // is the type of its argument is the same of its return type.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a => a2(a1(a))
    override def zero: A => A = a => a
  }

  // Ex 10.4
  // Use the property-based testing framework we developed in part 2 to
  // implement a property for the monoid laws. Use your property to test
  // the monoids we’ve written.
  // def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  // Ex 10.5
  // Implement foldMap.
  // def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldLeft(m.zero)(m.op)
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero) { (b, a) => m.op(b, f(a)) }


}
