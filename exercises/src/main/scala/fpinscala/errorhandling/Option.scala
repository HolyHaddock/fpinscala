package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case none => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None =>    default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(x => Some(x)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def apply[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  import java.util.regex._
  
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] = 
    pattern(pat) map (p => (s: String) => p.matcher(s).matches) // The details of this API don't matter too much, but `p.matcher(s).matches` will check if the string `s` matches the pattern `p`.

  def mkMatcher_1(pat: String): Option[String => Boolean] = 
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)
  
  def doesMatch(pat: String, s: String): Option[Boolean] = 
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f => 
    mkMatcher(pat2) map     (g => 
    f(s) && g(s)))
  def variance(xs: Seq[Double]): Option[Double] =
    for {meanOfxSquared <- mean(xs.map(x => x*x))
         meanOfX <- mean(xs)} yield meanOfxSquared - (meanOfX * meanOfX)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap { a_ =>
                                                                             b.map { b_ => f(a_,b_)}}

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = sys.error("todo")

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight(Some(List.empty[A]): Option[List[A]]) {
    (opt, acc) => opt.flatMap( x => acc.map(x :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight(Some(List.empty[B]) : Option[List[B]]) {
    (item, acc) => acc.flatMap( ls => f(item).map(x => x +: ls))
  }
}