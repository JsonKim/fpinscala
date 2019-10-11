package chapter09
import chapter08.Gen
import chapter08.Prop
import chapter08.Prop._
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

  type Parser[+A] = Location => Result[A]

  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError =
      copy(stack = (loc,msg) :: stack)

    def label[A](s: String): ParseError =
      ParseError(latestLoc.map((_, s)).toList)

    def latestLoc: Option[Location] =
      latest map (_._1)

    def latest: Option[(Location, String)] =
      stack.lastOption
  }

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def addCommit(isCommited: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommited)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, m+n)
      case _ => this
    }
  }
  case class Success[+A](get: A, charConsumed: Int) extends Result[A]
  case class Failure[+A](get: ParseError, isCommited: Boolean) extends Result[A]

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    s => p1(s) match {
      case Failure(e, false) => p2(s)
      case r => r
    }

  def attemp[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  implicit def string(s: String): Parser[String] =
    (loc: Location) =>
      if (loc.input.startsWith(s, loc.offset)) Success(s, loc.offset + s.length)
      else Failure(loc.toError("Expected: " + s), true)

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n < 1) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    p1.flatMap(a => p2.map(b => (a, b)))

  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p1.flatMap(a => p2.map(b => f(a, b)))

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    s => p(s) match {
      case Success(a, n) => f(a)(s.advanceBy(n))
                              .addCommit(n != 0)
                              .advanceSuccess(n)
      case Failure(e, c) => Failure(e, c)
    }

  implicit def regex(r: Regex): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, msg))

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset+1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(msg: String): ParseError =
      ParseError(List((this, msg)))

    def advanceBy(n: Int): Location =
      copy(offset = offset + n)
  }

  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}
