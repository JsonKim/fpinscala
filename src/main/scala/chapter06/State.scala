package chapter06


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    (if (n < 0) -(n + 1) else n, rng2)
  }

  def double(rng: RNG): (Double, RNG) =  {
    val (n, rng2) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1.0), rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((n, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) =  {
    val ((n, d), r) = intDouble(rng)
    ((d, n), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    if (count <= 0) (Nil, rng)
    else {
      val (n, r1) = rng.nextInt
      val (l, r2) = ints(n-1)(r1)
      (n :: l, r2)
    }

  def ints_1(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count <= 0) (xs, r)
      else {
        val (x, r2) = rng.nextInt
        go(count-1, r2, x :: xs)
      }
    
    go(count, rng, Nil)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - (i % 2))

  def doubleViaMap(rng: RNG): Rand[Double] = 
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  def sequence_1[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((r, acc) =>
      rng => {
        val (a, rng2) = r(rng)
        val (b, rng3) = acc(rng2)
        (a :: b, rng3)
      })

  def ints_2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (x, rng2) = f(rng)
    g(x)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a =>
    map(rb)(b =>
      f(a, b)))

  def map2ViaFlatMap_1[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a =>
    flatMap(rb)(b =>
      unit(f(a, b))))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, newS) = this.run(s)
    (f(a), newS)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s1) = this.run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, newS) = this.run(s)
      f(a).run(newS)
    })

  def mapViaFlatMap[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2ViaFlatMap[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a =>
    sb.map(b =>
      f(a, b)))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def nonNegativeInt: Rand[Int] = State(rng => {
    val (n, rng2) = int.run(rng)
    (if (n < 0) -(n + 1) else n, rng2)
  })

  def double: Rand[Double] =  State(rng => {
    val (n, rng2) = nonNegativeInt.run(rng)
    (n / (Int.MaxValue.toDouble + 1.0), rng2)
  })

  def intDouble: Rand[(Int, Double)] = for {
    n <- int
    d <- double
  } yield (n, d)

  def intDouble_1: Rand[(Int, Double)] =
    int.flatMap(n => double.map(d => (n, d))) 

  def nonNegativeEven: Rand[Int] =
    nonNegativeInt.map(i => i - (i % 2))

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val u = (modify[Machine] _) compose update
    val m = inputs map u
    val seq = sequence(m)
    for {
      _ <- seq
      s <- get
    } yield {
      (s.coins, s.candies)
    }
  }

  def simulateMachine_1(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val u = (f => modify[Machine](f)) compose update
    val seq = sequence(inputs map u)
    seq.flatMap(x =>
    get.map(s =>
      (s.coins, s.candies)))
  }

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil: List[A]))((r, acc) => r.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def modifyViaFlatMap[S](f: S => S): State[S, Unit] =
    get.flatMap(s =>
    set(f(s)).map(_ =>
      ()
    ))

  def fn1[S](f: S => S): State[S, Unit] = State(s_ => {
    get.flatMap((s: S) =>
    set(f(s)).map(_ =>
      ()
    )).run(s_)
  })

  // State의 map, flatMap은 입력 상태가 아닌 반환값만 다룰 수 있다.
  // 따라서 입력 상태를 조회하기 위해서는 입력 상태를 반환값으로 돌려주는 함수가 필요하다.
  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def fn2: Rand[RNG] = for {
    s <- get
  } yield (s)
}
