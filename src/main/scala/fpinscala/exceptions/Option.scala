package fpinscala.exceptions

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(get) => Some(f(get))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(option => if (f(option)) Some(option) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option extends App {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aVal => b.map(bVal => f(aVal, bVal)))

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case ::(head, next) => head flatMap (headElement => sequence(next).map(xs => headElement :: xs))
    case Nil => Some(Nil)
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case ::(head, next) => f(head) flatMap (headElement => traverse(next)(f).map(xs => headElement :: xs))
    case Nil => Some(Nil)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(optionA => optionA)


}
