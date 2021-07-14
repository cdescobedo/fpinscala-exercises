// Example implementation of List
package chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // Exercise 3.2 Implement the function tail
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  // Exercise 3.3
  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Nil => Cons(head, Nil)
    case Cons(_, tail) => Cons(head, tail)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => if (n <= 1) t else drop(t, n - 1)
  }

  // Exercise 3.5
  // Changed the implementation to curry since it allows scala to infer type for A in f
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) => if (f(h)) dropWhile(t)(f) else Cons(h, t)
    case _ => l
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Error("Cannot use init on empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /* 
  Exercise 3.7
  When implementing product with foldRight we cannot immediately halt
  the recurscion and return zero since foldRight needs to traverse all
  the way to the end of the List.
  */

  /*
  Exercise 3.8
  If we do this we would just end up with with the original list
  */

  // Exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  def foldLeft[A](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(as:List[A], acc: B): B = as match {
      case Nil => Nil
      case Cons(h, t) => foldLeft(t, f(z, h)(f))
    }

    loop(as, z)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
