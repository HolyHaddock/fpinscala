import fpinscala.errorhandling.{Option => Opt, _}

// Exercise 4.1:

val o1 = Some("test")
val o2: Opt[String] = None
val o3 = Some("something else")

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

// Exercise 4.2:

Opt.variance(Seq(2.0, 3.0, 4.0))
Opt.variance(Seq())
