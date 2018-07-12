package io.chrisdavenport.cormorant.generic

import io.chrisdavenport.cormorant._
import shapeless._
import shapeless.labelled._

/**
  * Fully Automatic Derivation of Any Product Type
  **/
object auto {
  implicit val hnilWrite = semiauto.hnilWrite
  implicit def hlistWrite[H, T <: HList](
    implicit P: Put[H], W: Write[T]
  ): Write[H :: T] = semiauto.hlistWrite[H, T]
  implicit def deriveWrite[A, R](
    implicit gen: Generic.Aux[A, R],
    enc: Write[R]
  ): Write[A] = semiauto.deriveWrite[A, R]

  implicit val labelledWriteHNil = semiauto.labelledWriteHNil
  implicit def deriveByNameHList[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    P: Lazy[Put[H]],
    labelledWrite: LabelledWrite[T]
  ): LabelledWrite[FieldType[K, H] :: T] = semiauto.deriveByNameHList[K, H, T]
  implicit def deriveLabelledWrite[A, H <: HList](
    implicit gen: LabelledGeneric.Aux[A, H], 
    hlw: Lazy[LabelledWrite[H]]
  ) : LabelledWrite[A] = semiauto.deriveLabelledWrite[A, H]

  implicit val readHNil: Read[HNil] = semiauto.readHNil
  implicit def hlistRead[H, T <: HList](
    implicit G: Get[H], R : Lazy[Read[T]]
  ): Read[H :: T] = semiauto.hlistRead[H, T]
  implicit def deriveRead[A, R](
    implicit gen: Generic.Aux[A, R],
    R: Lazy[Read[R]]
  ): Read[A] = semiauto.deriveRead[A, R]

  implicit val labelledReadHNil : LabelledRead[HNil] = semiauto.labelledReadHNil
  implicit def deriveLabelledReadHList[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    P: Lazy[Get[H]],
    labelledRead: LabelledRead[T]
  ): LabelledRead[FieldType[K, H] :: T] = semiauto.deriveLabelledReadHList

  implicit def deriveLabelledRead[A, H <: HList](implicit gen: LabelledGeneric.Aux[A, H], hlw: Lazy[LabelledRead[H]])
    : LabelledRead[A] = semiauto.deriveLabelledRead

}