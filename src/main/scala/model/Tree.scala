package model

import Tree.{Branch, Leaf}

sealed trait Tree[+A] {
  def size: (Int, Int) =
    fold[A, (Int, Int)](_ => (0, 1))((l, r) => (1 + l._1 + r._1, l._2 + r._2))

  def maximum[AA >: A](implicit numeric: Numeric[AA]): AA =
    fold[AA, AA](identity)(numeric.max)

  def depth: Int = fold[A, Int](_ => 1)(Math.max(_, _) + 1)

  def map[B](f: A => B): Tree[B] =
    fold[A, Tree[B]](z => Leaf(f(z)): Tree[B])(Branch(_, _))

  def fold2[B](f: A => B)(g: (Tree[A], Tree[A]) => B): B = this match {
    case Leaf(value)         => f(value)
    case Branch(left, right) => g(left, right)
  }

  def fold[AA >: A, B](f: AA => B)(g: (B, B) => B): B =
    fold2(f)((left, right) => g(left.fold(f)(g), right.fold(f)(g)))
}

object Tree {
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}
