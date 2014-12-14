package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(a) => Right(f(a))
   case Left(e) => Left(e)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => f(a)
   case Left(e) => Left(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case right => right
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap {
   a => b.map { b => f(a,b) }
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es.foldRight(Right(List.empty[A]) : Either[E, List[A]]) {
    case (e, acc) => e.flatMap(b => acc.map(bs => b +: bs))
  }
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldRight(Right(List.empty[B]) : Either[E, List[B]]) {
    case (a, e) => e.flatMap(bs => f(a).map(b => b +: bs))
  }

  def apply[A](a: => A): Either[Exception, A] = try Right(a)
                                              catch { case e: Exception => Left(e) }

}


/* Exercise 4.8

To capture all the errors, you could make the Left side a collection of some kind - the equivalent of Either[List[E], A].
This would require changing mkPerson to return an Either[List[String], Person], and map2 to return Either[List[EE], A]. The errors would have to have a useful
lowest supertype - Exception or some such.
orElse, traverse and sequence would not be able to use any laziness - in order to capture all the errors, all the elements of the list would have to be evaluated.

 */