import fpinscala.errorhandling.{Option => Opt, _}
import Opt._
// Examples

val o1 = Some("hello")
val o2: Opt[String] = None
val o3 = Some(" world")

// Exercise 4.5:
traverse(List("1", "2", "3"))(x => Try(x.toInt))
traverse(List("1", "2", "three"))(x => Try(x.toInt))

// Exercise 4.4:
sequence(List(o1, o2, o3))
sequence(List(o1, o3))
// Exercise 4.3:

map2(o1, o2)(_ + _)
map2(o1, o3)(_ + _)
// Exercise 4.2:

variance(Seq(2.0, 3.0, 4.0))
variance(Seq())
// Exercise 4.1:
o1.map(_.toUpperCase)
o2.map(_.toUpperCase)
o1.getOrElse("default")
o2.getOrElse("default")

o1.flatMap(_ => o2)
o2.flatMap(_ => o1)
o1.flatMap(str => Some(str + " world"))

o2 orElse o1
o1 orElse o2
o1 orElse o3

o1 filter(_.forall(_.isLower))
o1 filter(_ == "treasure!")
