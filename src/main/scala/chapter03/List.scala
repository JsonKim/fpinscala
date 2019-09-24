package chapter03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead of empty list")
    case Cons (_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n-1)
      }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => l
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, length) => length + 1)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](as: List[A]): Int =
    foldLeft(as, 0)((length, _) => length + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((xs, x) => Cons(x, xs))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def appendRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def appendLeft[A](a1: List[A], a2: List[A]) =
    foldLeft(a1, (xs: List[A]) => xs)((id, x) => xs => id(Cons(x, xs)))(a2)

  def concat[A](xss: List[List[A]]): List[A] = 
    foldRight(xss, Nil: List[A])(append)

  def add1(ns: List[Int]): List[Int] =
    foldRight(ns, Nil: List[Int])((x, xs) => Cons(x+1, xs))

  def doubleToString(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((x, xs) => Cons(x.toString, xs))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if (p(h)) Cons(h, t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]) =
    concat(map(as)(f))

  def filterViaFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(h => if (p(h)) List(h) else Nil)

  def addTwoList(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addTwoList(t1, t2))
    case _ => Nil
  }

  def zipWith[A,B](l: List[A], r: List[A])(f: (A, A) => B): List[B] = (l, r) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _ => Nil
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean =
    (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _ => false
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
}
