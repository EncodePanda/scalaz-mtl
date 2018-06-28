package scalaz
package mtl

import scalaz._

trait MonadState[F[_], S] extends Serializable {
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

object MonadState {
  def apply[F[_], S](implicit ev: MonadState[F, S]): MonadState[F, S] = ev

}
