package model

trait FunctionK[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object FunctionK {
  type ~>[F[_], G[_]] = FunctionK[F, G]
}
