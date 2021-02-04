package fpinscala.exceptions


case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(value) => Left(value)
      case Right(value) => f(value)
    }


  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(value) => Right(value)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (thisElement => b.map(bElement => f(thisElement, bElement)))
}

object Either extends App {
  // 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case ::(head, next) => head flatMap (headElement => sequence(next).map(xs => headElement :: xs))
    case Nil => Right(Nil)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case ::(head, next) => f(head) flatMap (headElement => traverse(next)(f).map(xs => headElement :: xs))
    case Nil => Right(Nil)
  }

}
