package io.chrisdavenport.cormorant

import cats.data.Validated
import cats.data.ValidatedNel

trait Read[A]{
  def read(a: CSV.Row): Either[Error.DecodeFailure, A]
}

object Read {
  def apply[A](implicit ev: Read[A]): Read[A] = ev

  object cursor {
    private def optionIndexOf[A](xs: List[A])(a: A): Option[Int] = {
      val temp = xs.indexOf(a)
      if (temp == -1) Option.empty[Int]
      else Some(temp)
    }

    def atHeader(header: String)
                (headers: CSV.Headers, row: CSV.Row): ValidatedNel[Error.DecodeFailure, CSV.Field] = {
        optionIndexOf(headers.l)(CSV.Header(header)).fold(
          Validated.invalidNel[Error.DecodeFailure, Int](Error.DecodeFailure(s"Header $header not present in header: $headers for row: $row"))
        )(Validated.validNel[Error.DecodeFailure, Int])
          .andThen(i => atIndex(row, i))
    }

    def atIndex(row: CSV.Row, index: Int): ValidatedNel[Error.DecodeFailure, CSV.Field] = {
      row.l.drop(index - 1).headOption.fold(
        Validated.invalidNel[Error.DecodeFailure, CSV.Field](Error.DecodeFailure(s"Index $index not present in row: $row "))
      )(Validated.validNel[Error.DecodeFailure, CSV.Field])
    }

    def decodeAtHeader[A: Get](header: String)(headers: CSV.Headers, row: CSV.Row): ValidatedNel[Error.DecodeFailure, A] = 
      atHeader(header)(headers, row)
        .andThen(Get[A].get(_).fold(Validated.invalidNel, Validated.validNel))
  
    def decodeAtIndex[A: Get](row: CSV.Row, index: Int): ValidatedNel[Error.DecodeFailure, A] = 
      atIndex(row,index)
        .andThen(Get[A].get(_).fold(Validated.invalidNel, Validated.validNel))

  }

  def fromHeaders[A](f: (CSV.Headers, CSV.Row) => Either[Error.DecodeFailure, A])
                    (headers: CSV.Headers): Read[A] = new Read[A]{
      def read(a: CSV.Row): Either[Error.DecodeFailure, A] = f(headers, a)
    }
  
}