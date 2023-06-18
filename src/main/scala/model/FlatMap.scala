package model

trait FlatMap[F[_]] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def join[A](fa: F[F[A]]) = flatMap(fa)(identity)

  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
}
