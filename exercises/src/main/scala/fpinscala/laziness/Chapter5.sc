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

constant(2).take(3).toList

Stream.from(5).take(5).toList

Stream.fibs.take(10).toList

Stream.UsingUnfold.constant(10).take(5).toList
Stream.UsingUnfold.ones.take(5).toList
Stream.UsingUnfold.fibs.take(10).toList
Stream.UsingUnfold.from(10).take(5).toList

example.FromUnfold.map(_ - 5).toList

example.FromUnfold.take(3).toList

example.FromUnfold.takeWhile(_ != 3).toList

example.FromUnfold.zipWith(Stream(10, 9, 8, 7, 6, 5)).toList

example.FromUnfold.zipWith(Stream(1)).toList

example.FromUnfold.zipWith(Stream(1,2,3,4,5,6)).toList

example.FromUnfold.zipAll(Stream(1)).toList

example.FromUnfold.zipAll(Stream(1,2,3,4,5,6)).toList

example.startsWith(Stream(1,2,3))
example.startsWith(Stream(1,2,3,4,5,6))
example.startsWith(Stream(1,7))

Stream(1,2,3).tails.toList.map(_.toList)

example.scanRight(0) {_ + _ }.toList
