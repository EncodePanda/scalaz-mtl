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

object ApplicativeAsk {
  def apply[F[_], E](implicit ev: ApplicativeAsk[F, E]): ApplicativeAsk[F, E] = ev

  def ask[F[_], E](implicit ev: ApplicativeAsk[F, E]): F[E] = ev.ask

  def reader[F[_], E, A](f: E => A)(implicit ev: ApplicativeAsk[F, E]): F[A] = ev.reader(f)
}
