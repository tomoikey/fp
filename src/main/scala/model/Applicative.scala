package model

trait Applicative[F[_]] extends Functor[F] { self =>
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
      f: (A, B, C) => D
  ): F[D] = apply(map2(fa, fb)(f.curried(_)(_)))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] = apply(map3(fa, fb, fc)(f.curried(_)(_)(_)))(fd)

  def traverse[A, B](fa: List[A])(f: A => F[B]): F[List[B]] =
    fa.foldLeft(unit(List.empty[B]))((b, a) => map2(f(a), b)(_ :: _))

  def sequence[A](as: List[F[A]]): F[List[A]] = traverse(as)(identity)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    map(traverse(ofa.toList) { case (k, fv) => map(fv)(v => (k, v)) })(_.toMap)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // F[B => (A, B)]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def product[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = (F[x], G[x]) })#f] = new Applicative[
    ({
      type f[x] = (F[x], G[x])
    })#f
  ] {
    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

    override def apply[A, B](fab: (F[A => B], G[A => B]))(
        fa: (F[A], G[A])
    ): (F[B], G[B]) = {
      val (fabL, fabR) = fab
      val (faL, faR) = fa
      (self.apply(fabL)(faL), G.apply(fabR)(faR))
    }
  }

  def compose[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = F[G[x]] })#f] = new Applicative[
    ({
      type f[x] = F[G[x]]
    })#f
  ] {
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
      self.map2(fab, fa)((gab, ga) => G.apply(gab)(ga))

    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(
        f: (A, B) => C
    ): F[G[C]] = self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f(_, _)))
  }
}

object Applicative {
  val listApplicative: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def map2[A, B, C](fa: List[A], fb: List[B])(
        f: (A, B) => C
    ): List[C] = fa.zip(fb).map(f.tupled)
  }

  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(
        f: (A, B) => C
    ): Option[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))
  }

  def mapApplicative[K]: Applicative[({ type f[x] = Map[K, x] })#f] =
    new Applicative[({ type f[x] = Map[K, x] })#f] {
      override def unit[A](a: => A): Map[K, A] = Map()
    }
}
