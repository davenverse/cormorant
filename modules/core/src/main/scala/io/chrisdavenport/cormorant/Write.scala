package io.chrisdavenport.cormorant

// Should Headers Have Their Own Typeclass?
trait Write[A] {
  def write(a: A): CSV.Row
}

object Write {
  def apply[A](implicit ev: Write[A]): Write[A] = ev
}
