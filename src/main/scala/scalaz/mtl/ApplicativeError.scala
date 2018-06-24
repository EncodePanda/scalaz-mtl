package scalaz
package mtl

import scalaz._

trait ApplicativeError[F[_], E] {

  val applicative: Applicative[F]

  def raiseError[A](e: E): F[A]
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

  def handleError[A](fa: F[A])(f: E => A): F[A] = handleErrorWith(fa)(f andThen (applicative.point[A](_)))
  def attempt[A](fa: F[A]): F[E \/ A] = handleErrorWith[E \/ A](
    applicative.map(fa)(\/-(_))
  )(e => applicative.point(-\/(e)))

  def attemptT[A](fa: F[A]): EitherT[F, E, A] = EitherT(attempt(fa))

  def fromDisjunction[A](either: E \/ A): F[A] =
    either.fold(raiseError, applicative.point(_))
}

object ApplicativeError {
  def apply[F[_], E](implicit ev: ApplicativeError[F, E]): ApplicativeError[F, E] = ev
}
