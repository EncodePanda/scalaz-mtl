package scalaz
package mtl

import scalaz.{MonadState => _, _}

trait MonadState[F[_], S] {
  val monad: Monad[F]

  def get: F[S]

  def set(s: S): F[Unit]

  def inspect[A](f: S => A): F[A]

  def modify(f: S => S): F[Unit]
}

trait DefaultMonadState[F[_], S] extends MonadState[F, S] {
  def inspect[A](f: S => A): F[A] = monad.map(get)(f)
  def modify(f: S => S): F[Unit] = monad.bind(inspect(f))(set)
}

object MonadState extends MonadState0 {
  def apply[F[_], S](implicit ev: MonadState[F, S]): MonadState[F, S] = ev
}

trait MonadState0  extends MonadState1 {
  implicit def eitherTMonadState[F[_]: Monad: MonadState[?[_], S], S, E]
      : MonadState[EitherT[F, E, ?], S] = new DefaultMonadState[EitherT[F, E, ?], S] {
    val monad: Monad[EitherT[F, E, ?]] = Monad[EitherT[F, E, ?]]
    def get: EitherT[F, E, S] =
      MonadTrans[EitherT[?[_], E, ?]].liftM(MonadState[F, S].get)
    def set(s: S): EitherT[F, E, Unit] =
      MonadTrans[EitherT[?[_], E, ?]].liftM(MonadState[F, S].set(s))
  }
}

trait MonadState1 {
  implicit def stateTMonadState[F[_]:Monad, S]
      : MonadState[StateT[F, S, ?], S] =
    new DefaultMonadState[StateT[F, S, ?], S] {
    val monad: Monad[StateT[F, S, ?]] = Monad[StateT[F, S, ?]]
    def get: StateT[F, S , S] = StateT(s => Monad[F].point((s, s)))
    def set(s: S): StateT[F, S, Unit] = StateT(_ => Monad[F].point((s, ())))
  }
}
