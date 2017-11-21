package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
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

  // Exercise 3.7
  def foldRightShortCircuit[A,B](as: List[A], z: B, s: A, r: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => 
        if (x == s) r
        else f(x, foldRightShortCircuit(xs, z, s, r)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def foldRightUncurried[A, B](as: List[A], z: B, f: (A, B) => B): B = 
    foldRight(as, z)(f)

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

  def setHead[A](l: List[A], h: A): List[A] =
    Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if (f(h)) => dropWhile(t, f)
      case _ => l
    }

  def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = 
    dropWhile(l, f)

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((x,y) => 1 + y)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(acc: B, l: List[A]): B = 
      l match {
        case Nil => acc
        case Cons(h, t) => go(f(acc, h), t)
      }

    go(z, l)
  }

  def sumFL(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productFL(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def lengthFL[A](l: List[A]): Int = 
    foldLeft(l, 0)((x,y) => x + 1)

  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, Nil:List[A])((x,y) => Cons(y,x))

  // Incorrect
  def foldRightFL[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b,a) => f(a, b))

  // Incorrect
  def foldLeftFR[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    foldRight(l, z)((a,b) => f(b, a))

  def appendFR[A](a: List[A], b: List[A]): List[A] = 
    foldRight(a, b)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] = 
    foldRight(l, Nil:List[A])(append)

  def addOneNaive(l: List[Int]): List[Int] = 
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, addOne(t))
    }

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h,t) => Cons(h+1, t))

  def toString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = 
    foldRight(as, Nil: List[A])((h, t) => 
      if (f(h)) Cons(h, t)
      else t
    )

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] = 
    flatMap(as)(a => if(f(a)) List(a) else Nil)

  def addLists(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Cons(x, y), Cons(h, t)) => Cons(x+h, addLists(y, t))
      case (Cons(x, y), Nil) => a
      case (Nil, Cons(h, t)) => b
      case (Nil, Nil) => Nil
    }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = 
    (a, b) match {
      case (Cons(g, h), Cons(i, j)) => Cons(f(g, i), zipWith(h, j)(f))
      case _ => Nil
    }

}
