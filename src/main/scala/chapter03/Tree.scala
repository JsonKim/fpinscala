package chapter03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => size(l) + size(r)
    case Leaf(_) => 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(x) => x
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    case Leaf(_) => 0
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(x) => Leaf(f(x))
  }

  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(x) => f(x)
  }

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(1 + _ + _)

  def maximumViaFold(tree: Tree[Int]): Int =
    fold(tree)(a => a)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((l, r) => 1 + (l max r))

  def mapViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
