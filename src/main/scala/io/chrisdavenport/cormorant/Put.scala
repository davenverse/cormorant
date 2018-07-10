package io.chrisdavenport.cormorant

trait Put[A]{
  def put(a: A): CSV.Field
}

object Put {
  def apply[A](ev: Put[A]): Put[A] = ev
}