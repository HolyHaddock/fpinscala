import fpinscala.errorhandling.{Option => Opt, _}
import Opt._

// Examples
val e1: Either[Int, String] = Right("Hello")
val e2: Either[Int, String] = Left(35)
val e3: Either[Int, String] = Right(" world")
val e4: Either[String, String] = Left("hello")
e1.map(_.toUpperCase)
e2.map(_.toUpperCase)
e4.map(_.toUpperCase)

e1.flatMap(_ => e3)
e2.flatMap(_ => e3)
e1.flatMap(x => Right(x + " world"))

e2.orElse(e1)
e1.orElse(e2)
e1.orElse(e3)

e1.map2(e3) (_ + _)
// Examples
val o1 = Some("hello")
val o2: Opt[String] = None
val o3 = Some(" world")
// Exercise 4.5:
traverse(List("1", "2", "3"))(x => Opt(x.toInt))
traverse(List("1", "2", "three"))(x => Opt(x.toInt))
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
