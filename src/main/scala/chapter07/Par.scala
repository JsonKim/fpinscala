package chapter07
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object Par {
  type Par[A] = ExecutorService => Future[A]

  // 즉시 평가되어서 결과 a를 산출하는 계산을 생성.
  // 상수 값을 병렬 계산으로 승격
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  // run이 동시적으로 평가할 표현식 a를 감싼다
  // 평가 되지 않은 인수를 Par로 감싸고 병렬 평가 대상으로 표시
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 상수 값을 그대로 돌려줄 수 있는 Future의 구현
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone(): Boolean = true
    def get(timeout: Long, unit: TimeUnit): A = get
    def isCancelled(): Boolean = false
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  // 주어진 Par를 fork의 요청에 따라 병렬 계산을 수행하고 그 결과값을 추출함으로써 완전히 평가
  // 계산의 실제 실행
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // 두 병렬 계산의 결과들을 이항 함수로 조합
  // 병렬성 제어는 오직 fork함수만 담당한다는 설계상의 선택에 의해 f호출을 개별 논리적 쓰레드에서 평가하지 않는다.
  // f를 개별 쓰레드에서 평가하고 싶다면 fork(map2(a,b)(f)) 를 사용한다.
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => Map2Future(a(es), b(es), f)

  private case class Map2Future[A,B,C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile
    var cache: Option[C] = None

    def isDone: Boolean = cache.isDefined
    def isCancelled: Boolean = a.isCancelled || b.isCancelled
    def cancel(mayInterruptIfRunning: Boolean): Boolean =
      a.cancel(mayInterruptIfRunning) ||b.cancel(mayInterruptIfRunning)
    def get: C = get(Long.MaxValue, TimeUnit.NANOSECONDS)
    def get(timeout: Long, unit: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None => {
        val aStart = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val aEnd = System.nanoTime
        val remain = timeoutInNanos - (aEnd - aStart)
        val br = b.get(remain, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
      }
    }
  }

  // map은 map2로 구현 가능하나 반대는 불가능하다.
  // 이것은 map2가 map보다 더 강력하고 근본적인 기본수단임을 의미한다.
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // 이후에 run이 동시적으로 평가할 계산임을 표시. run에 강제 되어야 실제로 평가
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil))((h, t) => map2(h, t)(_ :: _))

  def sequence_1[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case (h :: t) => map2(h, fork(sequence_1(t)))(_ :: _)
    case (Nil) => unit(Nil)
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence_2[A](ps: List[Par[A]]): Par[List[A]] = 
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)
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
      map2(fork(sum_3(l)), fork(sum_3(r)))(_ + _)
    }
}
