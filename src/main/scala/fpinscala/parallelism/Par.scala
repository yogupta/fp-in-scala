package fpinscala.parallelism


import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.concurrent.duration.TimeUnit

object Par {

  type Par[A] = ExecutorService => Future[A]


  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled: Boolean = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def get[A](a: Par[A]): A = ???

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) =>
    es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  case class Map2Future[A, B, C](a: Future[A], b: Future[B])(f: (A, B) => C) extends Future[C] {

    // why is volatile used here?
    @volatile var cache: Option[C] = None

    def isDone: Boolean = cache.isDefined

    def isCancelled: Boolean = a.isCancelled || b.isCancelled

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(value) => value
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  /**
   * A function to convert any function A => B to one that evaluates its result asynchronously.
   *
   * @param f function
   * @tparam A Input type of f
   * @tparam B output type of f
   * @return function which evaluates its result asynchronously.
   */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A]))((el, acc) => map2(el, acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as.map(asyncF((a: A) => if (f(a)) List(a) else List.empty[A]))
    map(sequence(pars))(_.flatten)
  }

  def reduce[A](as: IndexedSeq[A], zero: A)(f: (A, A) => A): Par[A] = {
    def recur(as: IndexedSeq[A]): Par[A] = {
      if (as.isEmpty) {
        unit(zero)
      } else {
        val (leftSeq, rightSeq) = as.splitAt(as.length / 2)
        val left = fork(recur(leftSeq))
        val right = fork(recur(rightSeq))
        map2(left, right)(f)
      }
    }

    recur(as)
  }

  def parMax[A](as: IndexedSeq[A])(max: (A, A) => A): Par[A] = reduce(as, as.head)(max)

  // how can we remove the first Par from type?
  // In later chapter we come to know of flatMap and join.
  def parNumOfWords(xs: List[String]): Par[Par[Int]] = map(
    parMap(xs)(x => x.split(" ").length)
  )(a => reduce(a.toIndexedSeq, 0)(_ + _))

  def map3[A, B, C, D](ap: Par[A], bp: Par[B], cp: Par[C])(f: (A, B, C) => D): Par[D] = {
    val cd: Par[C => D] = map2(ap, bp)((a, b) => (c: C) => f(a, b, c))
    map2(cd, cp)((fn, c) => fn(c))
  }

  def map4[A, B, C, D, E](ap: Par[A], bp: Par[B], cp: Par[C], dp: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val ab: Par[C => D => E] = map2(ap, bp)((a, b) => (c: C) => (d: D) => f(a, b, c, d))
    val abc: Par[D => E] = map2(ab, cp)((fn, c) => fn(c))
    map2(abc, dp)((fn, d) => fn(d))
  }

  // similarly map5. not gonna implement, too tedious

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // 11.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val i = run(es)(n).get
      run(es)(choices(i))
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  // 11.12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  // this is basically a flatMap
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a: A = run(es)(pa).get
      run(es)(choices(a))
    }

  // 11.13
  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if (b) t else f)

  def choiceNViaChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(p)(idx => choices(idx))

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a: A = run(es)(pa).get
      run(es)(choices(a))
    }

  def join[A](a: Par[Par[A]]): Par[A] = es => {
    val j1: Par[A] = run(es)(a).get()
    val res: Future[A] = run(es)(j1)
    res
  }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(parA => parA)

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = join(map(pa)(f))
}