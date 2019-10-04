package chapter07

object Par {
  case class Par[A](value: A)

  def unit[A](a: A): Par[A] = Par(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = a.value

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = unit(f(run(a), run(b)))

  def fork[A](a: => Par[A]): Par[A] = a
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def sum_1(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = unit(sum(l))
      val sumR: Par[Int] = unit(sum(r))
      run(sumL) + run(sumR)
    }

  def sum_2(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sum_2(l), sum_2(r))(_ + _)
    }

  def sum_3(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(sum_2(l)), fork(sum_2(r)))(_ + _)
    }
}
