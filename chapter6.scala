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
}
