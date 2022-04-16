package io.chrisdavenport.cormorant.generic.internal

import io.chrisdavenport.cormorant._
import shapeless._
import shapeless.labelled._
import cats.syntax.all._

trait LabelledReadProofs extends LowPriorityLabelledReadProofs {
  implicit val labelledReadHNil: LabelledRead[HNil] = new LabelledRead[HNil] {
    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure,HNil] = 
      Right(HNil)
  }

}

private[internal] trait LowPriorityLabelledReadProofs 
  extends LowPriorityLabelledReadProofs1 {

    implicit def deriveLabelledReadHList[K <: Symbol, H, T <: HList](
      implicit witness: Witness.Aux[K],
      P: Get[H],
      labelledRead: Lazy[LabelledRead[T]]
  ): LabelledRead[FieldType[K, H] :: T] = new LabelledRead[FieldType[K, H] :: T] {
    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure, FieldType[K, H] :: T] = {
      val header = CSV.Header(witness.value.name)
      (
        Cursor.decodeAtHeader[H](header)(h, a).map(field[K](_)),
        labelledRead.value.read(a, h)
      )
        .parMapN(_ :: _)
    }
  }
}

private[internal] trait LowPriorityLabelledReadProofs1
  extends LowPriorityLabelledReadProofs2 {
  implicit def deriveLabelledRead2[H, T <: HList](
    implicit
    P: LabelledRead[H],
    labelledRead: Lazy[LabelledRead[T]]
  ): LabelledRead[H :: T] = new LabelledRead[H :: T] {

    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure, H :: T] = {
      (
        P.read(a, h),
        labelledRead.value.read(a, h)
      )
        .parMapN{
          case (h, t) => h :: t
        }
    }
  }
}

private[internal] trait LowPriorityLabelledReadProofs2 {
  implicit def deriveLabelledRead3[K <: Symbol, H, T <: HList](
    implicit
    P: LabelledRead[H],
    labelledRead: Lazy[LabelledRead[T]]
  ): LabelledRead[FieldType[K, H] :: T] = new LabelledRead[FieldType[K, H] :: T] {

    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure, FieldType[K, H] :: T] = {
      (
        P.read(a, h),
        labelledRead.value.read(a, h)
      )
        .parMapN{
          case (h, t) => field[K](h) :: t
        }
    }
  }
}