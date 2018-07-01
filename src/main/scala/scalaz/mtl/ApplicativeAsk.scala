package scalaz
package mtl

import scalaz._

trait ApplicativeAsk[F[_], E] {
  val applicative: Applicative[F]

  def ask: F[E]

  def reader[A](f: E => A): F[A]
}

trait DefaultApplicativeAsk[F[_], E] extends ApplicativeAsk[F, E]  {
  def reader[A](f: E => A): F[A] = applicative.map(ask)(f)
}

class TransApplicativeAsk[F[_]: Monad: ApplicativeAsk[?[_], S], S, T[_[_], _]: MonadTrans](implicit
  val applicative: Applicative[T[F, ?]]
) extends DefaultApplicativeAsk[T[F, ?], S] {
  def ask: T[F, S]  = MonadTrans[T].liftM(ApplicativeAsk[F, S].ask)
}


object ApplicativeAsk extends ApplicativeAsk0 {
  def apply[F[_], E](implicit ev: ApplicativeAsk[F, E]): ApplicativeAsk[F, E] = ev

  def ask[F[_], E](implicit ev: ApplicativeAsk[F, E]): F[E] = ev.ask

  def reader[F[_], E, A](f: E => A)(implicit ev: ApplicativeAsk[F, E]): F[A] = ev.reader(f)

  def constant[F[_]:Applicative, E](e: E): ApplicativeAsk[F, E] = new DefaultApplicativeAsk[F, E] {
    val applicative: Applicative[F] = Applicative[F]
    def ask: F[E] = applicative.point(e)
  }
}

trait ApplicativeAsk0 extends ApplicativeAsk1 {
  implicit def eitherTApplicativeAsk[F[_]: Monad: ApplicativeAsk[?[_], S], S, E]
    : ApplicativeAsk[EitherT[F, E, ?], S] =
    new TransApplicativeAsk[F, S, EitherT[?[_], E, ?]]
}

trait ApplicativeAsk1 {
  implicit def stateTApplicativeAsk[F[_]: Monad: ApplicativeAsk[?[_], S], S, E]
    : ApplicativeAsk[StateT[F, E, ?], S] =
    new TransApplicativeAsk[F, S, StateT[?[_], E, ?]]
}
