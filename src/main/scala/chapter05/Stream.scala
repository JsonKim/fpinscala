package chapter05

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
  }

  def toList_1: List[A] = foldRight(Nil: List[A])(_ :: _)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t() take (n-1))
    case _ => empty
  }

  def take_1(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t() take (n-1))
    // n이 1일 경우는 tail을 평가할 필요가 없기 때문에 최적화
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t() drop (n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, b) => p(x) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty[A])

  def append[AA >: A](xs: => Stream[AA]): Stream[AA] = {
    foldRight(xs)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def startsWith[B](s: Stream[B]): Boolean = 
    zipAll(this, s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s drop 1)
    } append Stream(empty)

  def hasSubsequence[A](sub: Stream[A]): Boolean =
    tails exists (_ startsWith sub)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
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
    // Stream.cons(a, constant(a))
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(cur: Int, next: Int): Stream[Int] =
      cons(cur, go(cur, cur+next))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = (f(z)) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case _ => empty
  }

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (cur, next) => Some(cur, (next, cur+next)) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def map[A,B](xs: Stream[A])(f: A => B): Stream[B] =
    unfold(xs) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def take[A](xs: Stream[A], n: Int): Stream[A] =
    unfold((xs, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _ => None
    }

  def takeWhile[A](xs: Stream[A], p: A => Boolean): Stream[A] =
    unfold(xs) {
      case Cons(h, t) if (p(h())) => Some(h(), t())
      case _ => None
    }

  def zipWith[A,B,C](l: Stream[A], r: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((l,r)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  // special case of `zipWith`
  def zip[A,B](s1: Stream[A], s2: Stream[B]): Stream[(A,B)] =
    zipWith(s1, s2)((_,_))

  def zipAll[A,B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(s1, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), _)            => Some((Some(h1()), None), (t1(), empty))
      case (_, Cons(h2, t2))            => Some((None, Some(h2())), (empty, t2()))
      case _ => None
    }
}
