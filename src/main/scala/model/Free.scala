package model
import Free.*
import model.FunctionK.~>
import model.Monad.freeMonad

trait Free[F[_]: Functor, A] {
  def pure[B](x: B): Free[F, B] = Pure(x)

  def map[B](f: A => B): Free[F, B] = flatMap(a => pure(f(a)))

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMapped(this, f)

  def mapK[G[_]: Functor](f: F ~> G): Free[G, A] =
    foldMap[[x] =>> Free[G, x]] {
      new ~>[F, [x] =>> Free[G, x]]:
        override def apply[A](fa: F[A]): Free[G, A] = Free.liftF(f(fa))
    }

  def foldMap[G[_]](f: FunctionK[F, G])(using G: Monad[G]): G[A] =
    G.tailRecM(this) {
      case Pure(a)          => G.unit(Right(a))
      case Suspend(sa)      => G.map(f(sa))(Right(_))
      case FlatMapped(c, g) => G.map(c.foldMap(f))(cc => Left(g(cc)))
    }
}

object Free {
  final case class Pure[F[_]: Functor, A](x: A) extends Free[F, A]

  final case class Suspend[F[_]: Functor, A](x: F[A]) extends Free[F, A]

  final case class FlatMapped[F[_]: Functor, A, B](
      b: Free[F, A],
      f: A => Free[F, B]
  ) extends Free[F, B]

  def pure[F[_]: Functor, A](a: A): Free[F, A] = Pure(a)
  def liftF[F[_]: Functor, A](fa: F[A]): Free[F, A] = Suspend(fa)
}
