package scalaz
package mtl

import scalaz._, Scalaz._

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

object ApplicativeError extends ApplicativeError0 {
  def apply[F[_], E](implicit ev: ApplicativeError[F, E]): ApplicativeError[F, E] = ev
}

trait ApplicativeError0 {
  implicit def eitherTApplicativeError[F[_]:Monad, E]: ApplicativeError[EitherT[F, E, ?], E] =
    new ApplicativeError[EitherT[F, E, ?], E]  {
      val applicative: Applicative[EitherT[F, E, ?]] = Applicative[EitherT[F, E, ?]]
      def raiseError[A](e: E): EitherT[F, E, A] = EitherT(Monad[F].point(-\/(e)))
      def handleErrorWith[A](fa: EitherT[F, E, A])(f: E => EitherT[F, E, A]): EitherT[F, E, A] =
        EitherT(Monad[F].bind(fa.run) {
          case -\/(e) => f(e).run
          case r => Monad[F].point(r)
        })
    }

}
