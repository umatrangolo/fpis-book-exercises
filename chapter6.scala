package fpis.state

trait Rng {
  def nextInt: (Int, Rng)
}

case class SimpleRng(seed: Long) extends Rng {
  def nextInt: (Int, Rng) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = SimpleRng(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }
}

object Random {
  type Rand[+A] = Rng => (A, Rng)

  // Ex 6.1
  // Write a function that uses RNG.nextInt to generate a random
  // integer between 0 and Int.maxValue (inclusive). Make sure to handle
  // the corner case when nextInt returns Int.MinValue, which doesn’t
  // have a non-negative counterpart.
  val nonNegativeInt: Rand[Int] = { rng =>
    val (rnd, rng2) = rng.nextInt
    (scala.math.abs(rnd), rng2)
  }

  val int = nonNegativeInt

  // Ex 6.2
  // Write a function to generate a Double between 0 and 1, not
  // including 1. Note: You can use Int.MaxValue to obtain the maximum
  // positive integer value, and you can use x.toDouble to convert an x:
  // Int to a Double.
  val double: Rand[Double] = { rng =>
    val (nextNonNegativeInt, nextRng) = nonNegativeInt(rng)
    if (nextNonNegativeInt == Int.MaxValue) double(nextRng) else (nextNonNegativeInt.toDouble / Int.MaxValue, nextRng)
  }

  // Ex 6.3
  // Write functions to generate an (Int, Double) pair, a (Double, Int)
  // pair, and a (Double, Double, Double) 3-tuple. You should be able to
  // reuse the functions you’ve already written.
  val intDouble: Rand[(Int, Double)] = { rng =>
    val (nextInt, nextRng) = nonNegativeInt(rng)
    ((nextInt, double(rng)._1), nextRng)
  }

  val doubleInt: Rand[(Double,Int)] = { rng =>
    val (id, nextRng) = intDouble(rng)
    ((id._2, id._1), nextRng)
  }

  def double3(rng: Rng): Rand[(Double,Double,Double)] = { rng =>
    val (d1, nextRng1) = double(rng)
    val (d2, nextRng2) = double(nextRng1)
    val (d3, nextRng3) = double(nextRng2)
    ((d1, d2, d3), nextRng3)
  }

  // Ex 6.4
  // Write a function to generate a list of random integers.
  def ints(count: Int)(rng: Rng): (List[Int], Rng) = {
    def gen(n: Int, rng: Rng, intermediate: List[Int]): (List[Int], Rng) = n match {
      case 0 => (intermediate, rng)
      case n => {
        val (nextInt, nextRng) = nonNegativeInt(rng)
        gen(n - 1, nextRng, nextInt :: intermediate)
      }
    }
    gen(count, rng, List.empty[Int])
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Ex 6.5
  // Use map to reimplement double in a more elegant way.
  def double2: Rand[Double] = map(nonNegativeInt) { int => if (int < Int.MaxValue) int.toDouble else (int - 1).toDouble }

  // Ex 6.6
  // Write the implementation of map2 based on the following
  // signature. This function takes two actions, ra and rb, and a
  // function f for combining their results, and returns a new action
  // that combines them.
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, nra) = ra(rng)
    val (b, nrb) = rb(rng)
    (f(a, b), nrb)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  // Ex 6.7
  // If you can combine two RNG transitions, you should be able to
  // combine a whole list of them. Implement sequence for combining a List
  // of transitions into a single transition. Use it to reimplement the
  // ints function you wrote before. For the latter,you can use the
  // standard library function List.fill(n)(x) to make a list with x
  // repeated n times.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs.foldLeft((List.empty[A], rng)) { (b, rn) =>
    val (nextA, nextRng) = rn(b._2)
    (nextA :: b._1, nextRng)
  }
  def ints2(count: Int)(rng: Rng): (List[Int], Rng) = sequence(List.fill[Rand[Int]](count){ rng => rng.nextInt })(rng)

  // Ex 6.8
  // Implement flatMap, and then use it to implement nonNegativeLessThan.
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def map2[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }
  def map22[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a => map(rb) { b => f(a, b) } }
}
