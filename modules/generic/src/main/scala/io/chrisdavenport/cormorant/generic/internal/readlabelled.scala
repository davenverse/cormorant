package io.chrisdavenport.cormorant.generic.internal

import io.chrisdavenport.cormorant._
import shapeless._
import shapeless.labelled._
import cats.implicits._

trait LabelledReadProofs extends LowPriorityLabelledReadProofs {
  implicit val labelledReadHNil: LabelledRead[HNil] = new LabelledRead[HNil] {
    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure,HNil] = 
      Right(HNil)
  }

  implicit def deriveLabelledReadHList[K <: Symbol, H, T <: HList](
      implicit witness: Witness.Aux[K],
      P: Lazy[Get[H]],
      labelledRead: LabelledRead[T]
  ): LabelledRead[FieldType[K, H] :: T] = new LabelledRead[FieldType[K, H] :: T] {
    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure, FieldType[K, H] :: T] = {
      implicit val getAvailable: Get[H] = P.value
      val header = CSV.Header(witness.value.name)
      (
        Cursor.decodeAtHeader[H](header)(h, a).map(field[K](_)),
        labelledRead.read(a, h)
      )
        .parMapN(_ :: _)
    }
  }

}

private[internal] trait LowPriorityLabelledReadProofs {
  implicit def deriveLabelledRead2[H, T <: HList](
    implicit
    P: Lazy[LabelledRead[H]],
    labelledRead: Lazy[LabelledRead[T]]
  ): LabelledRead[H :: T] = new LabelledRead[H :: T] {

    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure, H :: T] = {
      implicit val getAvailable: LabelledRead[H] = P.value
      (
        getAvailable.read(a, h),
        labelledRead.value.read(a, h)
      )
        .parMapN{
          case (h, t) => h :: t
        }
    }
  }
}