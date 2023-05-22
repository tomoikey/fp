package model

// モノイド則とは結合律と同一律の総称

/** モノイドは以下の要素で構成される
  * 1. 何らかの型A。
  * 2. A型の2つの値を受け取り、それらを1つにまとめる2項連想演算op。
  * 任意のx: A, y: A, z: Aに対して「op(op(x, y), z) == op(x, op(y, z))」が成り立つ。
  * 3. この演算の単位減であるzero: Aの値。
  * 任意のx: Aに対して「op(x, zero) == op(zero, x)」 == xが成り立つ。
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A](monoid: Monoid[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] = for {
        aa1 <- a1
        aa2 <- a2
      } yield monoid.op(aa1, aa2)

      override def zero: Option[A] = None
    }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a => a2(a1(a))

    override def zero: A => A = identity
  }

}
