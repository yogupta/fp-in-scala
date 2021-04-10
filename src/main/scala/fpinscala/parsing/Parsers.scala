package fpinscala.parsing

import fpinscala.testing.{Gen, Prop}

import scala.util.matching.Regex

object Parsers {
  type Parser[+A] = Location => Result[A]
}

trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e) => Failure(f(e))
    case _ => this
  }
}

case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

case class Failure(get: ParseError) extends Result[Nothing]

import Parsers._

trait Parsers {

  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  // Exercise 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0)
      succeed(List.empty)
    else
      map2(p, listOfN(n - 1, p))((x, xs) => x :: xs)
  }

  // Exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))((x, xs) => x :: xs) or succeed(List.empty)
  }

  // EXERCISE 9.8 Express map in terms of flatMap and/or other combinators
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))

  /**
   * Always succeeds with the value a
   */
  def succeed[A](a: A): Parser[A] = loc => Success(a, 0)

  /**
   * return the portion of the input string examined by the parser if successful
   *
   * @return
   */
  def slice[A](p: Parser[A]): Parser[String] = {
    loc =>
      p(loc) match {
        case f@Failure(_) => f
        case Success(get, charsConsumed) => Success(loc.input.slice(0, charsConsumed), charsConsumed)
      }
  }

  // Exercise 9.1
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _)

  // EXERCISE 9.7
  /** Sequences two parsers, running p1 and then p2, and returns
   * the pair of their results if both succeed
   */
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    p.flatMap(a => p2.map(b => (a, b)))
  }

  // EXERCISE 9.7
  def map2ViaFlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    p.flatMap(a => p2.map(b => f(a, b)))
  }

  // Exercise 9.1
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = (p ** p2).map(f.tupled)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /** In the event of failure, replaces the assigned message with e */
  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  /** In the event of failure, adds e to the error stack returned by p */
  def scope[A](msg: String)(p: Parser[A]): Parser[A] = {
    loc => p(loc).mapError(err => err.push(loc, msg))
  }

  /**
   * Delays committing to a parser.
   * committing to a parser => It will evaluate the parser and if it fails then it will
   * not evaluate the other parser eg: in `or` or `product`
   *
   * @return
   */
  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))
}

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption
}
