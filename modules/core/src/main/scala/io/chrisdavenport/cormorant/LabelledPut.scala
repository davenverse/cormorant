package io.chrisdavenport.cormorant

trait LabelledPut[A]{
  def header: CSV.Header
  def put(a: A): CSV.Field
}

object LabelledPut {
  def apply[A](implicit ev: LabelledPut[A]): LabelledPut[A] = ev

  def byHeader[A: Put](h: CSV.Header): LabelledPut[A] = 
    new LabelledPut[A]{
      def header: CSV.Header = h
      def put(a: A): CSV.Field = Put[A].put(a)
    }
}