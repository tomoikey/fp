package model

trait FoldAble[F[_]] {
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B =
    foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldMap(as)(identity)(m)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])((a, b) => a :: b)
}

object FoldAble {
  val foldAbleList: FoldAble[List] = new FoldAble[List] {
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
  }

  val foldAbleIndexedSeq: FoldAble[IndexedSeq] = new FoldAble[IndexedSeq] {
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
  }

  val foldAbleStream: FoldAble[LazyList] = new FoldAble[LazyList] {
    override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
  }

  val foldAbleTree: FoldAble[Tree] = new FoldAble[Tree] {
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as.fold2(f(z, _))((left, right) =>
        foldLeft(right)(foldLeft(left)(z)(f))(f)
      )

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as.fold2(f(_, z))((left, right) =>
        foldRight(left)(foldRight(right)(z)(f))(f)
      )
  }

  val foldAbleOption: FoldAble[Option] = new FoldAble[Option] {
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.fold(z)(f(z, _))

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      foldLeft(as)(z)((b, a) => f(a, b))
  }

}
