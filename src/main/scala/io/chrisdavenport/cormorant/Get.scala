package io.chrisdavenport.cormorant

trait Get[A]{
  def read(string: CSV.Field): Either[Error.DecodeFailure, A]
}

object Get {
  def apply[A](implicit ev: Get[A]): Get[A] = ev
}