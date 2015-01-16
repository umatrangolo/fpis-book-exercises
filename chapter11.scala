package fpis.monads

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def ditribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab) { _._1 }, map(fab) { _._2})
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa) { Left(_) }
    case Right(fb) => map(fb) { Right(_) }
  }
}

object Functors {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma) { a => unit(f(a)) }
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa) { a => map(fb) { b => f(a, b) } }

  // Ex 11.3
  // The sequence and traverse combinators should be pretty familiar to
  // you by now, and your implementations of them from various prior
  // chapters are probably all very similar. Implement them once and
  // for all on Monad[F].
  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    val z: F[List[A]] = unit(List.empty[A])
    lma.foldRight(z) { case (a, z) => map2(z, a) { (intermediate, a) => a :: intermediate } }
  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map { f })

  // Ex 11.4
  // Implement replicateM.
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = traverse((0 to n).toList) { i => ma }

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

}

import fpis.testing._
import fpis.parallelism.{ Par, Exs }
import fpis.parallelism.Par.Par
import fpis.laziness._

object Monads {

  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  // Ex 11.1
  // Write monad instances for Par, Parser, Option, Stream, and List.
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Exs.flatMap(fa)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap { f(_) }
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.cons(a, Stream.empty[A])
    def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = a :: Nil
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap { f(_) }
  }
}
