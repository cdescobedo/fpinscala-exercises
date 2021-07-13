// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorialTail(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n - 1)
  }

  // Exercise 2.1
  def fibTail(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int, prev: Int): Int = n match {
      case 0 => acc
      case _ => loop(n - 1, prev, prev + acc)
    }
    loop(n, 0, 1)
  }

  def fib(n: Int): Int = n match{
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 1) + fib(n - 2)
  }
  
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  // Polymorphic function
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

      loop(0)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n == as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }

    loop(0)
  }

  // Partial function
  def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

}