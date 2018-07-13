package io.chrisdavenport.cormorant

trait Read[A]{
  def read(a: CSV.Row): Either[Error.DecodeFailure, A]
}

object Read {
  def apply[A](implicit ev: Read[A]): Read[A] = ev

  def fromHeaders[A](f: (CSV.Headers, CSV.Row) => Either[Error.DecodeFailure, A])
                    (headers: CSV.Headers): Read[A] = new Read[A]{
      def read(a: CSV.Row): Either[Error.DecodeFailure, A] = f(headers, a)
    }
  
}