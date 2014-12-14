package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y   // this one!
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case nil => nil           // This could be a throw, making tail non-total, and less useful.
  }

  def drop[A](l: List[A], n: Int): List[A] = if (n == 0) l else drop(tail(l), n-1)

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def setHead[A](l: List[A])(h: A): List[A] = Cons(h, tail(l))

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, l) => l + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumLeft(l : List[Int]) = foldLeft(l, 0)(_ + _)
  def productLeft(l : List[Double]) = foldLeft(l, 1.0)(_ * _)
  def lengthLeft[A](l :List[A]) = foldLeft(l, 0)((len, _) => len + 1)

  def foldLeftUsingRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def linkChain(a: A, g: B => B) = (acc: B) => g(f(acc, a))
    foldRight(l, (b: B) => b)(linkChain)(z)
  }

  def appendUsingFold[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))

  def reverse[A](l : List[A]) = foldLeft(l, Nil: List[A])((listSoFar, item) => Cons(item,listSoFar))

  def flatten[A](ls: List[List[A]]) = foldRight(ls, Nil: List[A])(append)

  def add1(l: List[Int]) = foldRight(l, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  def stringify(l : List[Double]) = foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](l: List[A])(p: A => Boolean) = foldRight(l, Nil: List[A])((x, acc) => if (p(x)) Cons(x, acc) else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((x, acc) => append(f(x), acc))

  def filterUsingFlatmap[A](l : List[A])(p: A => Boolean) = flatMap(l)(x => if (p(x)) List(x) else Nil)

  def addTwoLists(l1: List[Int], l2 : List[Int]): List[Int] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addTwoLists(t1, t2))
    case _ => Nil
  }

  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _ => Nil
  }

  def hasSubSequence[A](haystack: List[A], needle: List[A]): Boolean = (haystack, needle) match {
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => hasSubSequence(t1, t2) || hasSubSequence(t1, needle)
    case (Cons(h1, t1), Cons(h2, t2)) => hasSubSequence(t1, needle)
    case (_, Nil) => true
    case (Nil, _) => false
  }
}