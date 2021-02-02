package fpinscala.datastructures

object ListObj {

  def fibonacci(n: Int) = {

    @annotation.tailrec
    def go(num: Int, prevPrev: Int, prev: Int): Int = {
      if (num == n) prev + prevPrev
      else go(num + 1, prev, prev + prevPrev)
    }

    go(2, 0, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {

    @annotation.tailrec
    def loop(i: Int): Int = {
      if (i >= as.length) -1
      else if (p(as(i))) i
      else loop(i + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop(i: Int): Boolean = {
      if (i >= as.length) true
      else if (!ordered(as(i - 1), as(i))) false
      else loop(i + 1)
    }

    loop(1)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    def g(b: B): C = {
      f(a, b)
    }

    g
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    def g(a: A): B => C = {
      def h(b: B): C = f(a, b)

      h
    }

    g
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    def g(a: A, b: B): C = f(a)(b)

    g
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    def h(a: A): C = f(g(a))

    h
  }

  // 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l.init
    else drop(l.tail, n - 1)
  }

  // 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case head :: Nil => List()
      case head :: rest => head :: init(rest)
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  // 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  // 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case head :: tail => foldLeft(tail, f(z, head))(f)
    }
  }

  // 3.11
  def lengthFoldLeft[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, y) => acc + 1)
  }


  // 3.14
  def append[A](list: List[A], el: A) = {
    foldLeft(list, List(el))((acc, el) => el :: acc)
  }

  def append[A](list1: List[A], list2: List[A]) = {
    foldLeft(list1, list2)((acc, el) => el :: acc)
  }

  // 3.15
  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, Nil: List[A])((acc, el) => append(acc.reverse, el))
  }

  // 3.16
  def add1ToList(list: List[Int]) = {
    foldRight(list, Nil: List[Int])((el, acc) => (el + 1) :: acc)
  }

  // 3.17
  def doubleToString(list: List[Double]) = {
    foldRight(list, Nil: List[String])((d, acc) => d.toString :: acc)
  }


  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((el, acc) => f(el) :: acc)
  }


  // 3.19
  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((el, acc) => if (p(el)) el :: acc else acc)

  // 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // 3.22
  def zipWith[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] = {
    (list1, list2) match {
      case (_, Nil) => Nil: List[C]
      case (Nil, _) => Nil: List[C]
      case (h1 :: t1, h2 :: t2) => f(h1, h2) :: zipWith(t1, t2)(f)
    }
  }

  // 3.21
  def addPairWise(list1: List[Int], list2: List[Int]): List[Int] = zipWith(list1, list2)(_ + _)

}
