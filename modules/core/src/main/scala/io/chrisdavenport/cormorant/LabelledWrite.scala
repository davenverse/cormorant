package io.chrisdavenport.cormorant

trait LabelledWrite[A] {
  def headers: CSV.Headers
  def write(a: A): CSV.Row
}

object LabelledWrite {
  def apply[A](implicit ev: LabelledWrite[A]): LabelledWrite[A] = ev

  def byHeaders[A: Write](h: CSV.Headers): LabelledWrite[A] =
    new LabelledWrite[A] {
      def headers: CSV.Headers = h
      def write(a: A): CSV.Row = Write[A].write(a)
    }
}
