package io.chrisdavenport.cormorant.generic

import io.chrisdavenport.cormorant._
import shapeless._
import shapeless.labelled._

object semiauto {
  implicit def baseWrite[A: Put]: Write[A :: HNil]= new Write[A :: HNil]{
    def write(a: A :: HNil): CSV.Row = CSV.Row(List(Put[A].put(a.head)))
  }
  implicit def hlistWrite[H, T <: HList](
    implicit P: Put[H], W: Write[T]
  ): Write[H :: T] = new Write[H :: T]{
    def write(a: H :: T): CSV.Row = 
      CSV.Row(Put[H].put(a.head) :: Write[T].write(a.tail).l)
  }

  def deriveWrite[A, R](
    implicit gen: Generic.Aux[A, R],
    enc: Write[R]
  ): Write[A] = new Write[A]{
    def write(a: A): CSV.Row = Write[gen.Repr].write(gen.to(a))
  }

  //
  implicit val labelledWriteHNil : LabelledWrite[HNil] = new LabelledWrite[HNil]{
    def headers: CSV.Headers = CSV.Headers(List())
      def write(a: HNil): CSV.Row = CSV.Row(List())
  }

  implicit def deriveByNameHList[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    P: Lazy[Put[H]],
    labelledWrite: LabelledWrite[T]
  ): LabelledWrite[FieldType[K, H] :: T] = 
    new LabelledWrite[FieldType[K, H] :: T]{
      def headers: CSV.Headers = CSV.Headers(CSV.Header(witness.value.name) :: LabelledWrite[T].headers.l)
      def write(a: FieldType[K, H] :: T): CSV.Row = {
        CSV.Row(P.value.put(a.head):: labelledWrite.write(a.tail).l)
      }
    }

  def deriveLabelledWrite[A, H <: HList](implicit gen: LabelledGeneric.Aux[A, H], hlw: Lazy[LabelledWrite[H]])
    : LabelledWrite[A] = new LabelledWrite[A]{
      val writeH: LabelledWrite[H] = hlw.value
      def headers: CSV.Headers = writeH.headers
      def write(a: A): CSV.Row = writeH.write(gen.to(a))
    }

}