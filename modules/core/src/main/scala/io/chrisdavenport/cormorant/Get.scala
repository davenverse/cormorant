package io.chrisdavenport.cormorant

import scala.util.Try

import cats.Functor
import cats.implicits._

trait Get[A] {
  def get(field: CSV.Field): Either[Error.DecodeFailure, A]
}

object Get {
  def apply[A](implicit ev: Get[A]): Get[A] = ev

  def by[A](f: CSV.Field => A): Get[A] = new Get[A] {
    def get(field: CSV.Field): Either[Error.DecodeFailure, A] =
      Either.right(f(field))
  }

  def tryOrMessage[A](f: CSV.Field => Try[A], failedMessage: CSV.Field => String): Get[A] =
    new Get[A] {
      def get(field: CSV.Field): Either[Error.DecodeFailure, A] =
        f(field).toOption
          .fold[Either[Error.DecodeFailure, A]](
            Either.left(Error.DecodeFailure.single(failedMessage(field))))(x => Either.right(x))
    }

  implicit val getFunctor: Functor[Get] = new Functor[Get] {
    def map[A, B](fa: Get[A])(f: A => B): Get[B] = new Get[B] {
      def get(field: CSV.Field): Either[Error.DecodeFailure, B] =
        fa.get(field).map(f)
    }
  }

}
