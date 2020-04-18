package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight( unit(List[A]()) )( (ma, mlb) => map2(ma, mlb)(_::_) )

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight( unit(List[B]()) )( (a, mlb) => map2(f(a), mlb)(_::_) )

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = 
    if (n <= 0) unit(List[A]())
    else map2(ma, replicateM(n-1, ma))(_::_)

  def replicateM_2[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  // replicate는 주어진 모나드의 맥락을 유지한다.
  // 만약 Option의 None이면 그대로 None이 유지 되고, 5개의 List가 주어진다면 5개의 List는 유지 된다.

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _)
      )
    }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  // (>=>) :: forall m a b c. Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
  // (>=>) f g= \a -> f a >>= g

  //       (f         >=> g)   >=> h  ==        f   >=> (      g   >=> h)
  // \a -> (      f   >=> g) a >>= h  ==        f   >=> (\b -> g b >>= h)
  // \a -> (\b -> f b >>= g) a >>= h  ==  \a -> f a >>= (\b -> g b >>= h)
  //       (\a -> f a >>= g)   >>= h  ==  \a -> f a >>= (\b -> g b >>= h)
  //       (        x >>= g)   >>= h  ==          x >>= (\b -> g b >>= h)

  // 항등법칙
  //     f   >=> unit  == f   ==      unit   >=> f
  // a # f   >=> unit  == f a ==  a # unit   >=> f
  //     f a >>= unit  == f a ==      unit a >>= f

  // bind :: m  a    -> (   a  -> m b) -> m b
  // bind :: m (m a) -> ((m a) -> m a) -> m a
  // join :: m (m a) -> m a
  // join mma = mma >>= (\ma -> id ma)
  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(_)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))

  def __compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => join(map(f(a))(g))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] =
      p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A) = ???
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  // class를 선언하고 내부에
  // type StateS[A] = State[S,A]
  // 를 만들어서 쓸 수도 있지만, 아래와 같이 type lambda를 사용하면 inline으로 처리할 수 있다.
  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State.unit(a)
    def flatMap[A, B](ma: State[S,A])(f: A => State[S,B]): State[S,B] =
      ma flatMap f
  } 

  val idMonad: Monad[Id] = ???

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???
  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

