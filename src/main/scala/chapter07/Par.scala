package chapter07

object Par {
  case class Par[A](value: A)

  // 즉시 평가되어서 결과 a를 산출하는 계산을 생성.
  // 상수 값을 병렬 계산으로 승격
  def unit[A](a: A): Par[A] = Par(a)
  // run이 동시적으로 평가할 표현식 a를 감싼다
  // 평가 되지 않은 인수를 Par로 감싸고 병렬 평가 대상으로 표시
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 주어진 Par를 fork의 요청에 따라 병렬 계산을 수행하고 그 결과값을 추출함으로써 완전히 평가
  // 계산의 실제 실행
  def run[A](a: Par[A]): A = a.value

  // 두 병렬 계산의 결과들을 이항 함수로 조합
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = unit(f(run(a), run(b)))

  // 이후에 run이 동시적으로 평가할 계산임을 표시. run에 강제 되어야 실제로 평가
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
