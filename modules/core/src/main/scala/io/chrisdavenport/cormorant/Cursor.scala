package io.chrisdavenport.cormorant

import cats.data.Validated

object Cursor {
  private def optionIndexOf[A](xs: List[A])(a: A): Option[Int] = {
    val temp = xs.indexOf(a)
    if (temp == -1) Option.empty[Int]
    else Some(temp)
  }

  def atHeader(header: CSV.Header)(
      headers: CSV.Headers,
      row: CSV.Row): Validated[Error.DecodeFailure, CSV.Field] = {
    optionIndexOf(headers.l)(header)
      .fold[Validated[Error.DecodeFailure, Int]](
        Validated.invalid(Error.DecodeFailure.single(
          s"Header $header not present in header: $headers for row: $row"))
      )(Validated.valid)
      .andThen(i => atIndex(row, i))
  }

  def atIndex(row: CSV.Row, index: Int): Validated[Error.DecodeFailure, CSV.Field] = {
    row.l
      .drop(index)
      .headOption
      .fold(
        Validated.invalid[Error.DecodeFailure, CSV.Field](
          Error.DecodeFailure.single(s"Index $index not present in row: $row "))
      )(Validated.valid[Error.DecodeFailure, CSV.Field])
  }

  def decodeAtHeader[A: Get](
      header: CSV.Header)(headers: CSV.Headers, row: CSV.Row): Validated[Error.DecodeFailure, A] =
    atHeader(header)(headers, row)
      .andThen(Get[A].get(_).fold(Validated.invalid, Validated.valid))

  def decodeAtIndex[A: Get](row: CSV.Row, index: Int): Validated[Error.DecodeFailure, A] =
    atIndex(row, index)
      .andThen(Get[A].get(_).fold(Validated.invalid, Validated.valid))
}
