package fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object Nonblocking {

  sealed trait Future[A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
    val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
    p(es) { a => ref.set(a); latch.countDown() } // Asynchronously set the result, and decrement the latch
    latch.await() // Block until the `latch.countDown` is invoked asynchronously
    ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def delay[A](a: => A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  /**
   * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
   * This will come in handy in Chapter 13.
   */
  def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
    def apply(k: A => Unit) = f(k)
  }

  /**
   * Helper function, for evaluating an action
   * asynchronously, using the given `ExecutorService`.
   */
  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            if (br.isDefined) eval(es)(cb(f(a, br.get)))
            else ar = Some(a)
          case Right(b) =>
            if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
            else br = Some(b)
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }


}