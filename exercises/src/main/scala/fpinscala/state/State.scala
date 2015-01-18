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

  import State.Rand
  import State._

  val int: Rand[Int] = State(_.nextInt)

  def mapUsingFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    s.flatMap (a => unit(f(a)))

  def nonNegativeInt: Rand[Int] = int.flatMap { x =>
      if (x == Int.MinValue) nonNegativeInt else unit(Math.abs(x))
  }

  def double(rng: RNG): (Double, RNG) = {
    val x = rng.nextInt
    (Math.abs(x._1.toDouble) / Math.abs(Int.MinValue.toDouble), x._2)
  }

  val doubleFromMap = nonNegativeInt.map {
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
    State[RNG, C]{
      (rng: RNG) =>
        val (a, rng2) = ra.run(rng)
        val (b, rng3) = rb.run(rng2)
        (f(a,b), rng3)
    }
  }

  def map2UsingFlatmap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    ra.flatMap { a =>  rb.map { b => f(a,b)} }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    State[RNG, List[A]] { (rng: RNG) =>
      fs.foldRight((List.empty[A], rng)) { (ra, b) =>
        val (la, rng2) = b
        val (a, rng3) = ra.run(rng2)
        (a +: la, rng3)
      }
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    nonNegativeInt.flatMap {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a,b)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B] { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for { s <- get
                                                   _ <- set(f(s)) } yield ()

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    State[S, List[A]] { (s: S) =>
      fs.foldLeft((List.empty[A], s)) { (b, sa) =>
        val (la, s2) = b
        val (a, s3) = sa.run(s2)
        (a +: la, s3)
      }
    }
  }

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def simulate(input: Input): State[Machine, Unit] = modify { m: Machine =>
      input match {
      case Coin if m.locked && m.candies > 0 => m.copy(locked = false, coins = m.coins + 1)
      case Turn if !m.locked => m.copy(locked = true, candies = m.candies - 1)
      case _ => m
    }}
    sequence(inputs.map(i => simulate(i))).flatMap {
      _ => get.map(m => (m.coins, m.candies))
    }
  }
}


object Test extends App {
  import fpinscala.state.RNG._

  val rng = Simple(Int.MinValue)

  println(RNG.nonNegativeInt.run(rng))
  println(RNG.double(rng))
  println(RNG.intDouble(rng))
  println(RNG.doubleInt(rng))
  println(RNG.double3(rng))
  println(RNG.ints(5)(rng))

  println(RNG.doubleFromMap.run(rng))

  println(RNG.sequence(List.fill(5){RNG.int}).run(rng))

  println(State.simulateMachine(List(Coin, Turn, Turn, Coin, Coin, Turn)).run(Machine(true, 5, 0)))

}