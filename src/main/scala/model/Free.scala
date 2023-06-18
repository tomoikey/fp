package model
import Free.{FlatMapped, Pure}
import model.FunctionK.~>

trait Free[F[_]: Functor, A] {
  def pure[B](x: B): Free[F, B] = Pure(x)

  def map[B](f: A => B): Free[F, B] = flatMap(a => pure(f(a)))

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMapped(this, f)

  def mapK[G[_]](f: F ~> G): Free[G, A] = {
    val aa = new (F ~> G) {
      override def apply[A](fa: F[A]): G[A] = ???
    }
    ???
  }

  def foldMap[G[_]](f: F ~> G)(using G: Monad[G]): G[A] = ???

}

object Free {
  final case class Pure[F[_]: Functor, A](x: A) extends Free[F, A]

  final case class Suspend[F[_]: Functor, A](x: F[A]) extends Free[F, A]

  final case class FlatMapped[F[_]: Functor, B, C](
      b: Free[F, B],
      f: B => Free[F, C]
  ) extends Free[F, C]

  def pure[F[_]: Functor, A](a: A): Free[F, A] = Pure(a)
  def liftF[F[_]: Functor, A](fa: F[A]): Free[F, A] = Suspend(fa)

//  final case class Free[F[_]: Functor, A](x: F[FreeM[F, A]])
//      extends FreeM[F, A] {
//    override def flatMap[B](f: A => FreeM[F, B]): FreeM[F, B] =
//      Free(Functor.map(x)(_.flatMap(f)))
//  }
}
