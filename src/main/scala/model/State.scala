package model

final case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(n => State.unit(f(n)))

  def map2[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] = for {
    one <- this
    two <- that
  } yield f(one, two)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, ss) = run(s)
    f(a).run(ss)
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.reverse
    .foldLeft[State[S, List[A]]](unit(Nil))((b, a) => a.map2(b)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
