package fpis.testing

import fpis.state._

case class Gen[A](sample: State[Rng, A])

object Props {

  // Ex 8.3
  // Assuming the following representation of Prop, implement && as a
  // method of Prop.
  trait Prop {
    def check: Boolean
    def &&(p: Prop): Prop = {
      val me = this.check
      val other = p.check
      new Prop { override val check = me && other }
    }
  }

  // Ex 8.4
  // Implement Gen.choose using this representation of Gen. It should
  // generate integers in the range start to stopExclusive. Feel free to
  // use functions you’ve already written.
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(Random.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
}
