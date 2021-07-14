object Chapter3 {
  // Exercise 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    // we could also error on Nil
    case Nil => Nil
    case _::ys => ys
  }

  // Exercise 3.3
  def setHeadUsingTail[A](xs: List[A], head: A): List[A] =
    head::tail(xs)

  def setHead[A](xs: List[A], head: A): List[A] = xs match {
    // we could also error on Nil
    case Nil => head::Nil
    case _::ys => head::ys
  }

  // Exercise 3.4
  def drop[A](xs: List[A], n: Int): List[A] =
    if (n <= 0) xs
    else
      xs match {
        case Nil => Nil
        case _::ys => drop(ys, n - 1)
      }

  // Exercise 3.5
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case x::xs => if (f(x)) dropWhile(xs, f) else x::xs
  }

  // 3.5: Solution from scala-exercises with type inference for f
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
  l match {
    case h::t if f(h) => dropWhile(t, f)
    case _ => l
  }

  // Exercise 3.6
  /* 
  cannot be implemented in constant time because a1->a2->a3->Nil
  is singly linked, therefore we have to traverse through the list
  in order to get to the last element. We know that we have arrived
  at the last element becuase its pointer references Nil a3->Nil.
  If this was a doubly linked list, we could start at the tail and
  just replace the its value with Nil and return that.
  */
  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => sys.error("init of empty List.")
    case _::Nil => Nil
    case x::xs => x::init(xs)
  }

  // Listing 3.2
  def foldRight[A, B](as: List[A], b: B)(f: (A, B) => B): B = as match {
    case Nil => b
    case a::as => f(a, foldRight(as, b)(f))
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // Exercise 3.9
  def length[A](xs: List[A]): Int =
    foldRight(xs, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  def foldLeft[A, B](as: List[A], b: B)(f: (A, B) => B): B = {
    @annotation.tailrec
    def loop(as: List[A], b: B): B = {
      if (as == Nil) b
      else loop(as.tail, f(as.head, b))
    }

    loop(as, b)
  }

  // 3.10: Solution from scala-exercises
  @annotation.tailrec
  def foldLeft2[A, B](as: List[A], b: B)(f: (A, B) => B): B = 
    as match {
      case Nil => b
      case a::as => foldLeft2(as, f(a, b))(f)
    }

  // Exercise 3.11
  def sumFoldLeft(xs: List[Int]): Int =
    foldLeft(xs, 0)(_ + _)

  def productFoldLeft(xs: List[Double]): Double =
    foldLeft(xs, 1.0)(_ * _)

  // Exercise 3.12
  def reverse[A](xs: List[A]): List[A] =
    foldLeft (xs, List[A]()) (_ :: _)

  // Exercise 3.13 (Hard come back to this)

  // Exercise 3.14
  def append[A](xs: List[A], ys: List[A]) =
    foldRight(xs, ys)(_ :: _)

  // Exercise 3.15
  def concat[A](xss: List[List[A]]): List[A] =
    foldRight(xss, List[A]())(append)

  // Exercise 3.16
  def transformOne(xs: List[Int]): List[Int] =
    foldRight (xs, List[Int]()) ((x, ys) => x + 1 :: ys)
    
  // Exercise 3.17
  def listToString(xs: List[Double]): List[String] =
    reverse(foldLeft(xs, List[String]())((x, ys) => x.toString() :: ys))

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    reverse(foldLeft(as, List[B]())((a, bs) => f(a) :: bs))
  
  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((a, as) => if (f(a)) a :: as else as)

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]) = ???
}
