sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  // Currying of the function 'f' is needed to help the Scala compiler to infer
  // the type of its arguments (e.g. _ + _ instead of _:Int + _:Int)
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ints: List[Int]) = foldRight(ints, 0)(_ + _)

  def product2(ints: List[Int]) = foldRight(ints, 1.0)(_ * _)

  // Ex 3.1
  // The result is 3 cause the matched case is the third one

  // Ex 3.2
  // Implement the function tail for removing the first element of a
  // List. Note that the function takes constant time. What are different
  // choices you could make in your implementation if the List is Nil?
  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(x, xs) => xs // O(1)
  }

  // Ex 3.2
  // Using the same idea, implement the function setHead for replacing
  // the first element of a List with a different value.
  def setHead[A](ls: List[A], head: A): List[A] = ls match { // O(1) in both cases
    case Nil => List(head)
    case Cons(x, xs) => Cons(head, tail(ls))
  }

  // Ex 3.4
  // Generalize tail to the function drop, which removes the first n
  // elements from a list. Note that this function takes time proportional
  // only to the number of elements being dropped—we don’t need to make a
  // copy of the entire List
  def drop[A](l: List[A], n: Int): List[A] = l match { // O(n)
    case Nil => Nil
    case Cons(x, xs) => if (n > 0) drop(tail(l), n - 1) else l
  }

  // Ex 3.5
  // Implement dropWhile, which removes elements from the List prefix as
  // long as they match a predicate.
  def dropWhile[A](l: List[A], f: A => Boolean, intermediate: List[A] = Nil): List[A] = l match { // O(n)
    case Nil => intermediate
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f, Cons(x, intermediate)) else dropWhile(xs, f, intermediate)
  }

  // Ex 3.6
  // Implement a function, init, that returns a List consisting of all
  // but the last element of a List. So, given List(1,2,3,4), init will
  // return List(1,2,3). Why can’t this function be implemented in
  // constant time like tail?
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of an empty List!")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // Ex 3.9
  // Compute the length of a list using foldRight.
  def length[A](as: List[A]): Int = foldRight(as, 0)((a, b) => b + 1) // O(n)

  // Ex 3.10
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Ex 3.11
  def sum3(ints: List[Int]) = foldLeft(ints, 0)(_ + _)
  def product3(ints: List[Int]) = foldLeft(ints, 1.0)(_ * _)

  // Ex 3.12
  // Write a function that returns the reverse of a list (given
  // List(1,2,3) it returns List(3,2,1)). See if you can write it using a
  // fold.
  def reverse[A](ls: List[A]): List[A] = foldLeft(ls, List[A]()) { (b, a) => Cons(a, b) }

  // Ex 3.14
  // Implement append in terms of either foldLeft or foldRight.
  def append[A](ls: List[A], elem: A): List[A] = foldRight(ls, Cons(elem, Nil)) { Cons(_, _) }
}
