package fpis.applicative

import fpis.monads._

trait Appicative[F[_]] extends Functor[F] {
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
