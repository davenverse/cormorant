package io.chrisdavenport.cormorant

import cats.syntax.all._
import _root_.enumeratum.{Enum, EnumEntry}

package object enumeratum {

  implicit final def enumeratumPut[A <: EnumEntry](implicit underlying: Put[String]): Put[A] =
    underlying.contramap(_.entryName)

  implicit final def enumeratumGet[A <: EnumEntry](implicit
      underlying: Get[String],
      en: Enum[A]
  ): Get[A] =
    (field: CSV.Field) =>
      underlying.get(field) match {
        case Right(t) =>
          en
            .withNameInsensitiveEither(t)
            .leftMap(e => Error.DecodeFailure.single(e.toString))
        case Left(d) => Either.left(d)
      }
}
