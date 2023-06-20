package model

trait StackSafeMonad[F[_]] extends Monad[F] {
  override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    flatMap(f(a)) {
      case Left(value)  => tailRecM(value)(f)
      case Right(value) => unit(value)
    }
}
