package fpinscala.strictnessAndLaziness


import fpinscala.strictnessAndLaziness.Stream.{cons, empty, _}

import scala.annotation.tailrec

sealed trait Stream[+A] {

  // EXERCISE 5.1
  def toList: List[A] = {
    @tailrec
    def go[B](stream: Stream[B], acc: List[B]): List[B] = stream match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }

    go(stream = this, List()).reverse
  }

  // EXERCISE 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def go[B](stream: Stream[B], count: Int): Stream[B] = stream match {
      case Cons(h, t) if n == count => Cons(h, t)
      case Cons(_, t) if n > count => go(t(), count + 1)
      case _ => Empty
    }

    go(stream = this, count = 0)
  }

  // EXERCISE 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) =>
      val ss = f(h(), t().foldRight(z)(f))
      println(ss)
      ss
    case _ => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((el, rest) => p(el) || rest)

  // EXERCISE 5.4 Implement forAll
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((el, rest) => p(el) && rest)

  // EXERCISE 5.5 Use foldRight to implement takeWhile.
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((el, rest) => if (p(el)) cons(el, rest) else empty)

  // 5.6 Implement headOption using foldRight.
  def headOption: Option[A] = foldRight(None: Option[A])((el, rest) => Some(el) orElse rest)

  // EXERCISE 5.7 Implement map, filter, append, and flatMap using foldRight.
  // The append method should be non-strict in its argument.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((head, tail) => cons(f(head), tail))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((head, tail) => if (p(head)) cons(head, tail) else tail)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((head, tail) => cons(head, tail))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((head, tail) => f(head) append tail)

  // EXERCISE 5.13
  def mapUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def takeWhileUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some((f(Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some((f(None, Some(h2())), (empty, t2())))
      case _ => None

    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAll(s2)((_, _))

  // EXERCISE 5.14: Implement startsWith using functions you’ve written. It should check if one Stream is a prefix of another.
  // For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty) forAll {
      case (h, h2) => h == h2
    }

  // EXERCISE 5.15: Implement tails using unfold.
  // For a given Stream, tails returns the Stream of suffixes of the input sequence, starting with the original Stream.
  // For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case stream => Some((stream, stream drop 1))
    }.append(Stream(empty))
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)


  // Exercise 5.16
  // https://github.com/fpinscala/fpinscala/blob/master/answerkey/laziness/16.answer.scala
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toString: String = (h() :: t().toList).toString()
}

case object Empty extends Stream[Nothing]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head: A = hd
    lazy val tail: Stream[A] = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


  // EXERCISE 5.11: Write a more general stream-building function called unfold.
  // It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  // EXERCISE 5.12: Write fibs, from, constant, and ones in terms of unfold.
  def fibsUnfold: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(el => Some((el, el + 1)))

  def constantUnfold(n: Int): Stream[Int] =
    unfold(n)(_ => Some((n, n)))

  def ones: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  //EXERCISE 5.8: Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // EXERCISE 5.9: Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // EXERCISE 5.10: Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }

    go(f0 = 0, f1 = 1)
  }

}