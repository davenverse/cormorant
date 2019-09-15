package io.chrisdavenport.cormorant.generic.internal

import io.chrisdavenport.cormorant._
import shapeless._
import cats.data._

trait WriteProofs extends LowPriorityWriteProofs {
  implicit def hnilWrite[H](implicit P: Put[H]): Write[H :: HNil] = new Write[H :: HNil] {
    def write(a: H :: HNil): CSV.Row = CSV.Row(NonEmptyList.one(P.put(a.head)))
  }

  

  implicit def hlistWrite[H, T <: HList](
      implicit P: Put[H],
      W: Write[T]
  ): Write[H :: T] = new Write[H :: T] {
    def write(a: H :: T): CSV.Row = {
      CSV.Row(NonEmptyList(Put[H].put(a.head), Write[T].write(a.tail).l.toList))
    }
  }
}

private[internal] trait LowPriorityWriteProofs {
  implicit def hlistWriteW[H, T <: HList](
    implicit WH: Write[H],
    W: Write[T]
  ): Write[H :: T] = new Write[H :: T]{
    def write(a: H :: T): CSV.Row = 
      CSV.Row(WH.write(a.head).l.concatNel(W.write(a.tail).l))
  }
}