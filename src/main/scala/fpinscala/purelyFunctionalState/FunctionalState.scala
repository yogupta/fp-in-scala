package fpinscala.purelyFunctionalState


import scala.annotation.tailrec

object FunctionalState {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {

    type Rand[+A] = RNG => (A, RNG)

    case class SimpleRNG(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
        val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
        val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
        (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      }
    }

    // Exercise 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (rnd, newRNG) = rng.nextInt
      if (rnd < 0)
        (-(rnd + 1), newRNG)
      else
        (rnd, newRNG)
    }

    // Exercise 6.2
    def double(rng: RNG): (Double, RNG) = {
      val (rndInt, newRNG) = nonNegativeInt(rng)
      (rndInt.toDouble / (Int.MaxValue.toDouble + 1), newRNG)
    }

    // Exercise 6.3 use existing functions
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (rndInt, newRNG1) = rng.nextInt
      val (rndDouble, newRNG2) = double(newRNG1)
      ((rndInt, rndDouble), newRNG2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((rndInt, rndDouble), newRNG2) = intDouble(rng)
      ((rndDouble, rndInt), newRNG2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (double1, newRNG1) = double(rng)
      val (double2, newRNG2) = double(newRNG1)
      val (double3, newRNG3) = double(newRNG2)

      ((double1, double2, double3), newRNG3)
    }

    // Exercise 6.4 Write a function to generate a list of random integers.
    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
      Range(0, count).foldLeft((List.empty[Int], rng))((accNewRNG: (List[Int], RNG), x) => {
        val (acc, newRNG) = accNewRNG
        val (rnd, newRNG1) = newRNG.nextInt
        (rnd :: acc, newRNG1)
      })

    def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def go(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = {
        if (count == 0) {
          (acc, rng)
        } else {
          val (rnd, newRNG1) = rng.nextInt
          go(count - 1, rnd :: acc)(newRNG1)
        }
      }

      go(count, List.empty[Int])(rng)
    }


    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

    // Exercise 6.4: Use map to reimplement double in a more elegant way. See exercise 6.2.
    def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

    // Exercise 6.4: Write the implementation of map2 based on the following signature.
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] = both(nonNegativeInt, double)
    val randDoubleInt: Rand[(Double, Int)] = both(double, nonNegativeInt)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.reverse.foldLeft(unit(List.empty[A]))((rands, r) => map2(r, rands)(_ :: _))

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

    // Exercise 6.8: Implement flatMap, and then use it to implement nonNegativeLessThan.
    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }

    // Exercise 6.9: Reimplement map and map2 in terms of flatMap
    def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

    def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

    def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

    def boolean(rng: RNG): (Boolean, RNG) =
      rng.nextInt match {
        case (i, rng2) => (i % 2 == 0, rng2)
      }

  }

  case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, newState) = run(s)
      f(a).run(newState)
    })

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

  }

  object State {
    type Rand[A] = State[RNG, A]

    def unit[ST, AA](a: AA): State[ST, AA] = State(s => (a, s))

    def sequence[S, A](sts: List[State[S, A]]): State[S, List[A]] =
      sts.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

}
