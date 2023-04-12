package io.chrisdavenport.cormorant

import cats.syntax.all._
trait Read[A] {
  def read(a: CSV.Row): Either[Error.DecodeFailure, A] =
    readPartial(a).map(_.fold(_._2, identity))
  // It either fails, returns a partial row that is left and an outcome,
  // or the final outcome if it consumed all input of the row.
  def readPartial(a: CSV.Row): Either[Error.DecodeFailure, Either[(CSV.Row, A), A]]
}

object Read {
  def apply[A](implicit ev: Read[A]): Read[A] = ev

  def fromHeaders[A](
      f: (CSV.Headers, CSV.Row) => Either[Error.DecodeFailure, A]
  )(headers: CSV.Headers): Read[A] = new Read[A] {
    def readPartial(a: CSV.Row): Either[Error.DecodeFailure, Either[(CSV.Row, A), A]] =
      f(headers, a).map(Either.right)
  }

}
