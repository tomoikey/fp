package model

import model.Monad.stateMonad

trait Traverse[F[_]] extends Functor[F] { self =>
  def traverse[A, B, G[_]](fa: F[A])(f: A => G[B])(implicit
      G: Applicative[G]
  ): G[F[B]]

  def sequence[A, G[_]](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(identity)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[A, B, Traverse.Id](fa)(f)(Traverse.idApplicative)

  def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[A, M, ({ type f[x] = Traverse.Const[M, x] })#f](as)(f)(
      Traverse.monoidApplicative(mb)
    )

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse(fa)(f)(stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)(a =>
      for {
        state <- State.get[S]
        (b, nextState) = f(a, state)
        _ <- State.set(nextState)
      } yield b
    ).run(s)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    mapAccum(ta, 0)((a, index) => ((a, index), index + 1))._1

  def toList[A](ta: F[A]): List[A] =
    mapAccum(ta, List.empty[A])((a, state) => ((), a :: state))._2.reverse

  def reverse[A](fa: F[A]): F[A] = traverseS(fa)(_ =>
    for {
      state <- State.get[List[A]]
      head :: tail = state
      _ <- State.set(tail)
    } yield head
  ).run(toList(fa).reverse)._1

  def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = traverseS(fa)(a =>
    for {
      state <- State.get[B]
      _ <- State.set(f(state, a))
    } yield ()
  ).run(z)._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(
      G: Applicative[G],
      H: Applicative[H]
  ): (G[F[B]], H[F[B]]) =
    traverse[A, B, ({ type f[x] = (G[x], H[x]) })#f](fa)(a => (f(a), g(a)))(
      G.product(H)
    )

  def compose[H[_]](H: Traverse[H]): Traverse[({ type f[x] = F[H[x]] })#f] =
    new Traverse[
      ({
        type f[x] = F[H[x]]
      })#f
    ] {
      override def traverse[A, B, G[_]](fa: F[H[A]])(f: A => G[B])(implicit
          G: Applicative[G]
      ): G[F[H[B]]] = self.sequence(self.map(fa)(a => H.sequence(H.map(a)(f))))
    }
}

object Traverse {
  type Id[A] = A
  private type Const[M, B] = M

  def monoidApplicative[M](
      m: Monoid[M]
  ): Applicative[({ type f[x] = Const[M, x] })#f] = new Applicative[
    ({
      type f[x] = Const[M, x]
    })#f
  ] {
    override def unit[A](a: => A): Const[M, A] = m.zero

    override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(
        f: (A, B) => C
    ): Const[M, C] = m.op(fa, fb)
  }

  val idApplicative: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): A = a
    override def apply[A, B](fab: A => B)(fa: A): B = fab(fa)
  }

  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[A, B, G[_]](fa: List[A])(f: A => G[B])(implicit
        G: Applicative[G]
    ): G[List[B]] =
      fa.foldRight(G.unit(List.empty[B]))((a, b) => G.map2(f(a), b)(_ :: _))
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[A, B, G[_]](fa: Option[A])(f: A => G[B])(implicit
        G: Applicative[G]
    ): G[Option[B]] =
      fa.fold[G[Option[B]]](G.unit(None))(a => G.map(f(a))(Some(_)))
  }

  def mapTraverse[K]: Traverse[({ type f[x] = Map[K, x] })#f] = new Traverse[
    ({
      type f[x] = Map[K, x]
    })#f
  ] {
    override def traverse[A, B, G[_]](fa: Map[K, A])(f: A => G[B])(implicit
        G: Applicative[G]
    ): G[Map[K, B]] = fa.foldLeft(G.unit(Map.empty[K, B])) { case (b, (k, a)) =>
      G.map2(b, f(a))((l, b) => l + (k -> b))
    }
  }
}
