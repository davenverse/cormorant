package io.chrisdavenport.cormorant

trait Get[A]{
  def get(string: CSV.Field): Either[Error.DecodeFailure, A]
}

object Get {
  def apply[A](implicit ev: Get[A]): Get[A] = ev
}