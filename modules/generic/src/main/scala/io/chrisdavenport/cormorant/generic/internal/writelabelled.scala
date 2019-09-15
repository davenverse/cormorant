package io.chrisdavenport.cormorant.generic.internal

import io.chrisdavenport.cormorant._
import shapeless._
import shapeless.labelled._
import cats.implicits._
import cats.data.NonEmptyList

trait LabelledWriteProofs {

  /** 
   * Base of Logical Induction for Put Systems.
   * 
   * This proves Given a Put Before an HNil, so a single value
   * within a product type. That we can serialize this field by name
   * and value into a CSV
   * 
   **/
  implicit def labelledWriteHNilPut[K <: Symbol, H](
    implicit witness: Witness.Aux[K],
    P: Lazy[Put[H]]
  ): LabelledWrite[FieldType[K, H] :: HNil] = 
    new LabelledWrite[FieldType[K, H] :: HNil] {
      def headers: CSV.Headers =
        CSV.Headers(NonEmptyList.one(CSV.Header(witness.value.name)))
      def write(a: FieldType[K, H] :: HNil): CSV.Row = 
        CSV.Row(NonEmptyList.one(P.value.put(a.head)))
    }

  /**
   * This is the logical extension of the above base induction
   * case Given som Field type with a name we serialize that field
   * as  
   *
   **/
  implicit def deriveByNameHListPut[K <: Symbol, H, T <: HList](
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
}

private[internal] trait LowPriorityLabelledWriteProofs {
  
  implicit def deriveByLabelledWrite2[K, H, T <: HList](
    implicit P: Lazy[LabelledWrite[H]],
    labelledWrite: Lazy[LabelledWrite[T]]
  ): LabelledWrite[FieldType[K, H] :: T] =
    new LabelledWrite[FieldType[K, H]:: T] {
      def headers: CSV.Headers = {
        CSV.Headers(
          P.value.headers.l <+>
          labelledWrite.value.headers.l
        )
      }
      def write(a: FieldType[K, H] :: T): CSV.Row = {
        CSV.Row(P.value.write(a.head).l.concatNel(labelledWrite.value.write(a.tail).l))
      }
    }
}