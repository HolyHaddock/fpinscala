import fpinscala.laziness._
import Stream._

val example = Stream(1,2,3,4,5)

example.toList

empty.toList

example.foldRight(0)(_ + _)
example.take(3).toList
example.drop(3).toList

example.takeWhile(_ != 3).toList
example.takeWhile2(_ != 3).toList

example.forAll(_ > 3)
example.forAll(_ < 6)

empty.headOption
example.headOption

example.map(_ - 5).toList

example.filter(_ % 2 == 0).toList

example.append(Stream(6,7,8)).toList

example.flatMap(i => Stream(i*2, i*3, i*4)).toList