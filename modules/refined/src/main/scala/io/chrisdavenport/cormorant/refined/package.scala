package io.chrisdavenport.cormorant

import cats.syntax.all._
import eu.timepit.refined.api.{RefType, Validate}

package object refined {

  implicit final def refinedPut[T, P, F[_, _]](implicit
      underlying: Put[T],
      refType: RefType[F]
  ): Put[F[T, P]] = underlying.contramap(refType.unwrap)

  implicit final def refinedGet[T, P, F[_, _]](implicit
      underlying: Get[T],
      validate: Validate[T, P],
      refType: RefType[F]
  ): Get[F[T, P]] = new Get[F[T, P]] {
    def get(field: CSV.Field): Either[Error.DecodeFailure, F[T, P]] =
      underlying.get(field) match {
        case Right(t) =>
          refType.refine(t) match {
            case Left(err) => Either.left(Error.DecodeFailure.single(err))
            case Right(ftp) => Either.right(ftp)
          }
        case Left(d) => Either.left(d)
      }

  }
}
