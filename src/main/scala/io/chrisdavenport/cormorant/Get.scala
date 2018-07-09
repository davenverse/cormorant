package io.chrisdavenport.cormorant

trait Get[A]{
  def read(string: CSV.Field): Either[ParseFailure, A]
}

object Get {

}