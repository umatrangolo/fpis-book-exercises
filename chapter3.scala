import scala.annotation._

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

  // Ex 3.15
  // Write a function that concatenates a list of lists into a single
  // list. Its runtime should be linear in the total length of all
  // lists. Try to use functions we have already defined.
  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys) { Cons(_, _) }
  def concatenate[A](lls: List[List[A]]): List[A] = foldRight(lls, List[A]()) { append(_, _) }

  // Ex 3.16
  // Write a function that transforms a list of integers by adding 1 to
  // each element. (Reminder: this should be a pure function that returns
  // a new List!)
  def inc(ls: List[Int]): List[Int] = foldRight(ls, List[Int]()) { (a, b) => Cons(a + 1, b) }

  // Ex 3.17
  // Write a function that turns each value in a List[Double] into a
  // String. You can use the expression d.toString to convert some d:
  // Double to a String.
  def _toString(ls: List[Double]): List[String] = foldRight(ls, List[String]()) { (a, b) => Cons(a.toString, b) }

  // Ex 3.18
  // Write a function map that generalizes modifying each element in a
  // list while maintaining the structure of the list.
  def map[A,B](as: List[A])(f: A => B): List[B] = reverse(foldLeft(as, List[B]()) { (b, a) => Cons(f(a), b) })

  // Ex 3.19
  // Write a function filter that removes elements from a list unless
  // they satisfy a given predicate. Use it to remove all odd numbers
  // from a List[Int].
  def filter[A](as: List[A])(f: A => Boolean): List[A] = reverse(foldLeft(as, List[A]()) { (b, a) => if (f(a)) Cons(a, b) else b })

  // Ex 3.20
  // Write a function flatMap that works like map except that the
  // function given will return a list instead of a single result, and
  // that list should be inserted into the final resulting list. For
  // instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
  // List(1,1,2,2,3,3).
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = reverse(concatenate(foldLeft(as, List[List[B]]()) { (b, a) => Cons(f(a), b) }))

  // Ex 3.21
  // Use flatMap to implement filter.
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as) { a => if (f(a)) Cons(a, Nil) else Nil  }

  // Ex 3.21/3.22
  // Write a function that accepts two lists and constructs a new list
  // by adding corresponding elements. For example, List(1,2,3) and
  // List(4,5,6) become List(5,7,9).
  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = {
    def loop(left: List[A], right: List[B], intermediate: List[C]): List[C] = (left, right) match {
      case (Nil, Nil) => intermediate
      case (rest, Nil) => intermediate
      case (Nil, rest) => intermediate
      case (Cons(leftHead, leftTail), Cons(rightHead, rightTail)) => loop(leftTail, rightTail, Cons(f(leftHead, rightHead), intermediate))
    }

    reverse(loop(xs, ys, List[C]()))
  }

  // Ex 3.24
  // As an example, implement hasSubsequence for checking whether a List
  // con- tains another List as a subsequence. For instance,
  // List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as
  // subsequences, among others.
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(ls: List[A], prefix: List[A]): Boolean = (ls, prefix) match {
      case (_, Nil) => true
      case (Cons(head, tail), Cons(head2, tail2)) if (head == head2) => startsWith(tail, tail2)
      case _ => false
    }

    sup match {
      case Nil => false
      case Cons(x, xs) if startsWith(sup, sub) => true
      case Cons(x, xs) => hasSubsequence(xs, sub)
    }
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Ex 3.25
  // Write a function size that counts the number of nodes (leaves and
  // branches) in a tree.
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  // Ex 3.26
  // Write a function maximum that returns the maximum element in a
  // Tree[Int].
  def maximum(tree: Tree[Int]): Int = {
    def _maximum(tree: Tree[Int], intermediate: Int): Int = tree match {
      case Leaf(v) => intermediate.max(v)
      case Branch(left, right) => _maximum(left, intermediate).max(_maximum(right, intermediate)).max(intermediate)
    }

    _maximum(tree, 0)
  }
}
