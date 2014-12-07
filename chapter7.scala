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
}
