package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
    case _ => empty
  }
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty: Stream[A]) {
    (a, acc) => if (p(a)) cons(a, acc) else empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true) {
    (a, acc) => p(a) && acc
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def headOption = foldRight(None: Option[A])((a, _) => Some(a))
  
  def toList: List[A] = foldRight(Nil: List[A]){ (x, acc) => x :: acc }

  def map[B](f: A => B) = foldRight(empty[B]) {
    (a, acc) => cons(f(a), acc)
  }

  def filter(p: A => Boolean) = foldRight(empty[A]) {
    (a, acc) => if (p(a)) cons(a, acc) else acc
  }

  def flatMap[B](f: A => Stream[B]) = foldRight(empty[B])((a, acc) => f(a).append(acc))

  def append[B>:A](other: =>Stream[B]) = foldRight(other)((a, acc) => cons(a, acc))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val result: Stream[A] = Stream.cons(a, result)
    result
  }

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}