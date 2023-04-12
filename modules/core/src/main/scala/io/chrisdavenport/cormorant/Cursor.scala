package io.chrisdavenport.cormorant

import cats.syntax.all._

object Cursor {
  private def optionIndexOf[A](xs: List[A])(a: A): Option[Int] = {
    val temp = xs.indexOf(a)
    if (temp == -1) Option.empty[Int]
    else Some(temp)
  }

  def atHeader(
      header: CSV.Header
  )(headers: CSV.Headers, row: CSV.Row): Either[Error.DecodeFailure, CSV.Field] = {
    optionIndexOf(headers.l.toList)(header)
      .fold[Either[Error.DecodeFailure, Int]](
        Either.left(
          Error.DecodeFailure
            .single(s"Header $header not present in header: $headers for row: $row")
        )
      )(Either.right)
      .flatMap(i => atIndex(row, i))
  }

  def atIndex(row: CSV.Row, index: Int): Either[Error.DecodeFailure, CSV.Field] = {
    row.l.toList
      .drop(index)
      .headOption
      .fold(
        Either.left[Error.DecodeFailure, CSV.Field](
          Error.DecodeFailure.single(s"Index $index not present in row: $row ")
        )
      )(Either.right[Error.DecodeFailure, CSV.Field])
  }

  def decodeAtHeader[A: Get](
      header: CSV.Header
  )(headers: CSV.Headers, row: CSV.Row): Either[Error.DecodeFailure, A] =
    atHeader(header)(headers, row)
      .flatMap(Get[A].get(_))

  def decodeAtIndex[A: Get](row: CSV.Row, index: Int): Either[Error.DecodeFailure, A] =
    atIndex(row, index)
      .flatMap(Get[A].get(_))
}
