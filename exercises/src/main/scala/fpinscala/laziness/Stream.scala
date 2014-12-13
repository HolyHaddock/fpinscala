package fpinscala.laziness

import Stream._

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  def take(n: Int): Stream[A] = uncons match {
    case Some((h, t)) if n > 0 => cons(h, t.take(n-1))
    case _ => empty
  }
  def drop(n: Int): Stream[A] = uncons match {
    case Some((h, t)) if n > 0 => t.drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some((h, t)) if p(h) => cons(h, t.takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = uncons match {
    case Some((h, t)) if p(h) => t.forAll(p)
    case None => true
    case _ => false
  }

  def toList: List[A] = foldRight(Nil: List[A]){ (x, acc) => x :: acc }
}
object Stream {
  def empty[A]: Stream[A] = 
    new Stream[A] { def uncons = None }
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
    new Stream[A] {
      lazy val uncons = Some((hd, tl)) 
    }
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = sys.error("todo")
}