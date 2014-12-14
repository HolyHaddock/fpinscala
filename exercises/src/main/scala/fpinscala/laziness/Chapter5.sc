import fpinscala.laziness._
import Stream._

val example = Stream(1,2,3,4,5)

example.toList

empty.toList

example.take(3).toList
example.drop(3).toList

example.takeWhile(_ != 3).toList
example.takeWhile2(_ != 3).toList

example.forAll(_ > 3)
example.forAll(_ < 6)

