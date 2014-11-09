package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def example: Tree[Int] = Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(4))), Leaf(2))

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }


  def fold[A, B](t: Tree[A])(leafOp: A => B)(combiningOp: (B, B) => B): B = t match {
    case Leaf(v) => leafOp(v)
    case Branch(l, r) => combiningOp(fold(l)(leafOp)(combiningOp), fold(r)(leafOp)(combiningOp))
  }

  def sizeInFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _)

  def maximumInFold[A](t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depthInFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ max _)

  def mapInFold[A, B](t: Tree[A])(f: A=> B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])(Branch.apply[B])

}