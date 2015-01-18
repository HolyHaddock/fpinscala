package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapUsingFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val x = rng.nextInt
    if (x._1 == Int.MinValue) nonNegativeInt(x._2) else (Math.abs(x._1), x._2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val x = rng.nextInt
    (Math.abs(x._1.toDouble) / Math.abs(Int.MinValue.toDouble), x._2)
  }

  val doubleFromMap = map(nonNegativeInt) {
    i => i.toDouble / Math.abs(Int.MinValue.toDouble)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (d, resultRng) = double(nextRng)
    ((i, d), resultRng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, nextRng) = double(rng)
    val (i, resultRng) = nextRng.nextInt
    ((d, i), resultRng)
  }

  def double3(rng1: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng1)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List.empty, rng) else {
      val (i, nextRng) = rng.nextInt
      val (l, resultRng) = ints(count - 1)(nextRng)
      (i +: l, resultRng)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (rng: RNG) =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
  }

  def map2UsingFlatmap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>  map(rb) { b => f(a,b)} }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    (rng: RNG) =>
      fs.foldRight((List.empty[A], rng)) { (ra, b) =>
        val (la, rng2) = b
        val (a, rng3) = ra(rng2)
        (a +: la, rng3)
      }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    (rng: RNG) =>
      val (a, rng2) = f(rng)
      g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    RNG.flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}


object Test extends App {
  import fpinscala.state.RNG._

  val rng = Simple(Int.MinValue)

  println(RNG.nonNegativeInt(rng))
  println(RNG.double(rng))
  println(RNG.intDouble(rng))
  println(RNG.doubleInt(rng))
  println(RNG.double3(rng))
  println(RNG.ints(5)(rng))

  println(RNG.doubleFromMap(rng))

  println(RNG.sequence(List.fill(5){RNG.int})(rng))
}