package io.chrisdavenport.cormorant.generic.internal

import io.chrisdavenport.cormorant._
import shapeless._
import shapeless.labelled._
import cats.implicits._
import cats.data.NonEmptyList

trait LabelledWriteProofs {

  implicit def deriveByNameHList[K <: Symbol, H, T <: HList](
      implicit witness: Witness.Aux[K],
      P: Lazy[Put[H]],
      labelledWrite: LabelledWrite[T]
  ): LabelledWrite[FieldType[K, H] :: T] =
    new LabelledWrite[FieldType[K, H] :: T] {
      def headers: CSV.Headers = {
        CSV.Headers(
          NonEmptyList.one(CSV.Header(witness.value.name)) <+>
          LabelledWrite[T].headers.l
        )
      }
      def write(a: FieldType[K, H] :: T): CSV.Row =
        CSV.Row(P.value.put(a.head) :: labelledWrite.write(a.tail).l)
    }

  implicit def labelledWriteHNil[K <: Symbol, H](implicit witness: Witness.Aux[K],
      P: Lazy[Put[H]]): LabelledWrite[FieldType[K, H] :: HNil] = new LabelledWrite[FieldType[K, H] :: HNil] {
    def headers: CSV.Headers = CSV.Headers(NonEmptyList.one(CSV.Header(witness.value.name)))
    def write(a: FieldType[K, H] :: HNil): CSV.Row = CSV.Row(NonEmptyList.one(P.value.put(a.head)))
  }
}

private[internal] trait LowPriorityLabelledWriteProofs {
  implicit def deriveByLabelledWrite2[H, T <: HList](
    implicit 
    P: Lazy[LabelledWrite[H]],
    labelledWrite: Lazy[LabelledWrite[T]]
  ): LabelledWrite[H :: T] =
    new LabelledWrite[H:: T] {
      def headers: CSV.Headers = {
        CSV.Headers(
          P.value.headers.l <+>
          labelledWrite.value.headers.l
        )
      }
      def write(a: H :: T): CSV.Row = {
        CSV.Row(P.value.write(a.head).l.concatNel(labelledWrite.value.write(a.tail).l))
      }
    }
}