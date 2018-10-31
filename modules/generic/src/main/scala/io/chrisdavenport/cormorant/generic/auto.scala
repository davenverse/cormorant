package io.chrisdavenport.cormorant.generic

import io.chrisdavenport.cormorant._
import shapeless._
import shapeless.labelled._

/**
 * Fully Automatic Derivation of Any Product Type
  **/
object auto {
  implicit def hnilWrite[H](implicit P: Put[H]): Write[H :: HNil] = semiauto.hnilWrite[H]
  implicit def hlistWrite[H, T <: HList](
      implicit P: Put[H],
      W: Write[T]
  ): Write[H :: T] = semiauto.hlistWrite[H, T]
  implicit def deriveWrite[A, R](
      implicit gen: Generic.Aux[A, R],
      enc: Write[R]
  ): Write[A] = semiauto.deriveWrite[A, R]

  implicit def labelledWriteHNil[K <: Symbol, H](implicit witness: Witness.Aux[K],
      P: Lazy[Put[H]]) = semiauto.labelledWriteHNil
  implicit def deriveByNameHList[K <: Symbol, H, T <: HList](
      implicit witness: Witness.Aux[K],
      P: Lazy[Put[H]],
      labelledWrite: LabelledWrite[T]
  ): LabelledWrite[FieldType[K, H] :: T] = semiauto.deriveByNameHList[K, H, T]
  implicit def deriveLabelledWrite[A, H <: HList](
      implicit gen: LabelledGeneric.Aux[A, H],
      hlw: Lazy[LabelledWrite[H]]
  ): LabelledWrite[A] = semiauto.deriveLabelledWrite[A, H]

  implicit def readHNil[H](implicit G: Get[H]): Read[H :: HNil] = semiauto.readHNil[H]
  implicit def hlistRead[H, T <: HList](
      implicit G: Get[H],
      R: Lazy[Read[T]]
  ): Read[H :: T] = semiauto.hlistRead[H, T]
  implicit def deriveRead[A, R](
      implicit gen: Generic.Aux[A, R],
      R: Lazy[Read[R]]
  ): Read[A] = semiauto.deriveRead[A, R]

  implicit val labelledReadHNil: LabelledRead[HNil] = semiauto.labelledReadHNil
  implicit def deriveLabelledReadHList[K <: Symbol, H, T <: HList](
      implicit witness: Witness.Aux[K],
      P: Lazy[Get[H]],
      labelledRead: LabelledRead[T]
  ): LabelledRead[FieldType[K, H] :: T] = semiauto.deriveLabelledReadHList

  implicit def deriveLabelledRead[A, H <: HList](
      implicit gen: LabelledGeneric.Aux[A, H],
      hlw: Lazy[LabelledRead[H]]): LabelledRead[A] = semiauto.deriveLabelledRead

}
