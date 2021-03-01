package fpinscala.testing

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.purelyFunctionalState.FunctionalState.{RNG, State}
import fpinscala.strictnessAndLaziness.Stream
import fpinscala.testing.Gen._
import fpinscala.testing.Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (maxSize, testCases, rng) =>
      run(maxSize, testCases, rng) match {
        case Passed => p.run(maxSize, testCases, rng)
        case f => f
      }
  }

  def ||(p: Prop): Prop = Prop {
    (maxSize, testCases, rng) =>
      run(maxSize, testCases, rng) match {
        case Falsified(failure, successes) => p.tag(failure).run(maxSize, testCases, rng)
        case pass => pass
      }
  }

  def tag(msg: String): Prop = Prop {
    (maxSize, n, rng) =>
      run(maxSize, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }

}


object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int


  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))


  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(as)(rng)
        .zip(Stream.from(0))
        .take(n)
        .map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
        }
        .find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props
          .map(p => Prop {
            (max, n, rng) => p.run(max, casesPerSize, rng)
          })
          .toList
          .reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  val S: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  ) // `a -> b` is syntax sugar for `(a,b)`


  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
    val parGen: Gen[(ExecutorService, A)] = S.map2(g)((_, _))

    forAll(parGen) { case (es, a) => f(a)(es).get }
  }


  val pint2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 20)).map(l =>
    l.foldLeft(Par.unit(0))(
      (p, i) =>
        Par.fork {
          Par.map2(p, Par.unit(i))((a, b) => a + b)
        })
  )

}

object ** {
  def unapply[A, B](p: (A, B)): Some[(Any, Any)] = Some(p)
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val list: State[RNG, List[A]] = State.sequence(List.fill(n)(g.sample))
    Gen(list)
  }

  // 8.4 Implement Gen.choose using this representation of Gen. It should generate integers in the range start to stopExclusive.
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val state: State[RNG, Int] = State(RNG.double).map(n => (start + (stopExclusive - start) * n).toInt)
    Gen(state)
  }

  val boolean: Gen[Boolean] = Gen(State(RNG.boolean))


  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  // 8.8 Implement weighted, a version of union that accepts a weight for each Gen and generates
  // values from each Gen with probability proportional to its weight.
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, w1) = g1
    val (gen2, w2) = g2

    val gen1Probability = w1 / (w1 + w2)
    val sample = State(RNG.double).flatMap(value => if (value < gen1Probability) gen1.sample else gen2.sample)
    Gen(sample)
  }

}

case class Gen[+A](sample: State[RNG, A]) {
  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val _sample: State[RNG, B] = sample.flatMap(a => f(a).sample)
    Gen(_sample)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  // 8.10 Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g) ((_, _))
}


case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g: Int => Gen[B] = n => {
      forSize(n) flatMap {
        f(_).forSize(n)
      }
    }
    SGen(g)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))

  val smallInt = Gen.choose(-10, 10)

  val maxProp = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n max 1, g))

  val maxProp1 = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  // We specify that every sorted list is either empty, has one element,
  // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
  val sortedProp = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
  }

}
