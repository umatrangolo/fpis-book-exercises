package fpis.parallelism

import java.util.concurrent.TimeUnit

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}

trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (ex: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](es: ExecutorService)(pa: Par[A]): Future[A] = pa(es)

  // Ex 7.1
  def map2[A, B, C](pa: => Par[A], pb: => Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = pa(es)
    val bf = pb(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](pa: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call = pa(es).get
  })
}

object Exs {
  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] = if (ints.size <= 1) {
    Par.unit(ints.headOption getOrElse 0)
  } else {
    val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(sum(l), sum(r))(_ + _)
  }

  // Ex 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(())) { (a, _) => a.sorted }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(())) { (a, _) => f(a) }
  def sortPar2(parList: Par[List[Int]]): Par[List[Int]] = map(parList) { _.sorted }

  // Ex 7.5
  // Write this function, called sequence. No additional primitives are required. Do not call run.
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequence(t))) { _ :: _ }
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map { asyncF(f) }
    sequence(fbs)
  }

  // Ex 7.6
  // Implement parFilter, which filters elements of a list in parallel.
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pars: List[Par[List[A]]] = as.map { asyncF((a: A) => if (f(a)) List(a) else List()) }
    map(sequence(pars))(_.flatten)
  }

  // Ex 7.11
  // Implement choiceN and then choice in terms of choiceN.
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = ex => {
    val choice = run(ex)(n).get
    run(ex)(choices(choice))
  }
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val n: Par[Int] = map(cond) { c => if (c) 0 else 1 }
    choiceN(n)(List(t, f))
  }

  // Ex 7.12
  // There’s still something rather arbitrary about choiceN. The choice
  // of List seems overly specific. Why does it matter what sort of
  // container we have? For instance, what if, instead of a list of
  // computations, we have a Map of them:
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = ex => {
    val k: K = run(ex)(key).get
    run(ex)(choices(k))
  }

}
