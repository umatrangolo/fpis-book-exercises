package fpis.applicative

import fpis.monads._

trait OldApplicative[F[_]] extends Functor[F] {
  // primitive
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(())) { (a, _) => f(a) }
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]())) { (a, fbs) => map2(f(a), fbs) { _ :: _ } }

  // Ex 12.1
  // Transplant the implementations of as many combinators as you can
  // from Monad to Applicative, using only map2 and unit, or methods
  // implemented in terms of them.
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas) { fa => fa }
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence((0 to n).toList.map { i => fa })
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb) { (a, b) => (a, b) }
}

trait Applicative[F[_]] extends Functor[F] {

  // Ex 12.2
  // The name applicative comes from the fact that we can formulate the
  // Applicative interface using an alternate set of primitives, unit and
  // the function apply, rather than unit and map2. Show that this
  // formulation is equivalent in expressiveness by defining map2 and map
  // in terms of unit and apply. Also establish that apply can be imple-
  // mented in terms of map2 and unit.
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa) { f.curried })(fb)
}
