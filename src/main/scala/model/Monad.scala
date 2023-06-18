package model

import scala.annotation.tailrec

// Monad則
// 結合律 compose(compose(f, g), h) == compose(f, compose(g, h))
// 同一律 compose(unit, f) == compose(f, unit) == f
trait Monad[F[_]] extends FlatMap[F] with Applicative[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    flatMap(fa)(a => map(fab)(ab => ab(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as match {
      case Nil => unit(Nil)
      case head :: tail =>
        flatMap(f(head))(b =>
          if (b) map2(unit(head), filterM(tail)(f))(_ :: _)
          else filterM(tail)(f)
        )
    }

  // compose(f, unit) == f
  // compose(unit, f) == f
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  //    def flatMapViaJoin[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  //    def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
  //      compose[Unit, A, B](_ => fa, f)(())
  //     def composeViaJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
  //      a => join(map(f(a))(g))

  def composeM[G[_]](
      G: Monad[G],
      T: Traverse[G]
  ): Monad[({ type f[x] = F[G[x]] })#f] = new Monad[
    ({
      type f[x] = F[G[x]]
    })#f
  ] {
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
      self.flatMap(fa)(ga => self.map(T.sequence(G.map(ga)(f))(self))(G.join))

    override def tailRecM[A, B](a: A)(f: A => F[G[Either[A, B]]]): F[G[B]] =
      flatMap(f(a)) {
        case Right(value) => unit(value)
        case Left(value)  => tailRecM(value)(f)
      }
  }
}

object Monad {
  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = {
      val buf = List.newBuilder[B]
      @tailrec def go(lists: List[List[Either[A, B]]]): Unit =
        lists match {
          case (ab :: abs) :: tail =>
            ab match
              case Right(b) =>
                buf += b
                go(abs :: tail)
              case Left(a) => go(f(a) :: abs :: tail)
          case Nil :: tail => go(tail)
          case Nil         => ()
        }
      go(f(a) :: Nil)
      buf.result()
    }
  }
  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match
        case None               => None
        case Some(Right(value)) => Some(value)
        case Some(Left(value))  => tailRecM(value)(f)
  }

  def eitherMonad[E]: Monad[
    ({
      type f[x] = Either[E, x]
    })#f
  ] = new Monad[({ type f[x] = Either[E, x] })#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(
        f: A => Either[E, B]
    ): Either[E, B] = fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(
        f: A => Either[E, Either[A, B]]
    ): Either[E, B] = f(a) match
      case Right(Right(value)) => Right(value)
      case Right(Left(value))  => tailRecM(value)(f)
      case Left(value)         => Left(value)
  }

  def stateMonad[S]: Monad[({ type f[x] = State[S, x] })#f] = new Monad[
    ({
      type f[x] = State[S, x]
    })#f
  ] {
    override def flatMap[A, B](fa: State[S, A])(
        f: A => State[S, B]
    ): State[S, B] = fa.flatMap(f)

    override def unit[A](a: => A): State[S, A] = State.unit(a)

    override def tailRecM[A, B](a: A)(
        f: A => State[S, Either[A, B]]
    ): State[S, B] = f(a).flatMap {
      case Right(value) => unit(value)
      case Left(value)  => tailRecM(value)(f)
    }
  }
}
