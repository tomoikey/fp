package model

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def map[F[_], A, B](fa: F[A])(f: A => B)(using functor: Functor[F]): F[B] =
    functor.map(fa)(f)
  given Functor[Option] with
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
}
