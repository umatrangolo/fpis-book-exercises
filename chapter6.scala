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
  // Ex 6.1
  // Write a function that uses RNG.nextInt to generate a random
  // integer between 0 and Int.maxValue (inclusive). Make sure to handle
  // the corner case when nextInt returns Int.MinValue, which doesn’t
  // have a non-negative counterpart.
  def nonNegativeInt(rng: Rng): (Int, Rng) = {
    val (rnd, rng2) = rng.nextInt
    (scala.math.abs(rnd), rng2)
  }

  // Ex 6.2
  // Write a function to generate a Double between 0 and 1, not
  // including 1. Note: You can use Int.MaxValue to obtain the maximum
  // positive integer value, and you can use x.toDouble to convert an x:
  // Int to a Double.
  def double(rng: Rng): (Double, Rng) = {
    val (nextNonNegativeInt, nextRng) = nonNegativeInt(rng)
    if (nextNonNegativeInt == Int.MaxValue) double(nextRng) else (nextNonNegativeInt.toDouble / Int.MaxValue, nextRng)
  }

  // Ex 6.3
  // Write functions to generate an (Int, Double) pair, a (Double, Int)
  // pair, and a (Double, Double, Double) 3-tuple. You should be able to
  // reuse the functions you’ve already written.
  def intDouble(rng: Rng): ((Int,Double), Rng) = {
    val (nextInt, nextRng) = nonNegativeInt(rng)
    ((nextInt, double(rng)._1), nextRng)
  }
  def doubleInt(rng: Rng): ((Double,Int), Rng) = {
    val (id, nextRng) = intDouble(rng)
    ((id._2, id._1), nextRng)
  }
  def double3(rng: Rng): ((Double,Double,Double), Rng) = {
    val (d1, nextRng1) = double(rng)
    val (d2, nextRng2) = double(nextRng1)
    val (d3, nextRng3) = double(nextRng2)
    ((d1, d2, d3), nextRng3)
  }
}
