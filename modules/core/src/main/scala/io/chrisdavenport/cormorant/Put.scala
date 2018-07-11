package io.chrisdavenport.cormorant

import cats.Contravariant

trait Put[A]{
  def put(a: A): CSV.Field
}

object Put {
  def apply[A](implicit ev: Put[A]): Put[A] = ev

  def by[A](f: A => CSV.Field): Put[A] =
    new Put[A]{
      def put(a: A): CSV.Field = f(a)
    }

  implicit val getContravariant: Contravariant[Put] = new Contravariant[Put] {
    def contramap[A, B](fa: Put[A])(f: B => A): Put[B] =  new Put[B]{
      def put(a: B): CSV.Field = fa.put(f(a))
    }
  }
}