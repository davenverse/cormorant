package io.chrisdavenport.cormorant

trait Read[A]{
  def read(a: CSV.Row): Either[Error.DecodeFailure, A]
}

object Read {
  def apply[A](implicit ev: Read[A]): Read[A] = ev
}