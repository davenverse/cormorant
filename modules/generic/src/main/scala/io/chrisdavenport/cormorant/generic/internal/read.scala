package io.chrisdavenport.cormorant.generic.internal

import io.chrisdavenport.cormorant._
import shapeless._
import cats.syntax.all._
import cats.data.NonEmptyList

trait ReadProofs extends LowPriorityReadProofs {

  implicit def readHNil[H](implicit G: Get[H]): Read[H :: HNil] = new Read[H :: HNil] {
    def readPartial(a: CSV.Row): Either[Error.DecodeFailure, Either[(CSV.Row, H :: HNil), H :: HNil]] = a match {
      case CSV.Row(NonEmptyList(f, Nil)) => 
        G.get(f).map(h => h :: HNil).map(Either.right)
      case CSV.Row(NonEmptyList(f, rest)) => 
        NonEmptyList.fromList(rest) match {
          case Some(nel) => 
            G.get(f).map(h => h :: HNil)
              .map(h => Either.left((CSV.Row(nel), h)))
          case None =>
            Either.left(Error.DecodeFailure.single(s"Unexpected Input: Did Not Expect - $a"))
        }
    }
  }

  implicit def hlistRead[H, T <: HList](
      implicit G: Get[H],
      R: Lazy[Read[T]]
  ): Read[H :: T] = new Read[H :: T] {
    def readPartial(a: CSV.Row): Either[Error.DecodeFailure, Either[(CSV.Row, H :: T), H :: T]] = a match {
      case CSV.Row(NonEmptyList(h, t)) =>
        (
          G.get(h),
          NonEmptyList.fromList(t)
          .fold(
            Either.left[Error.DecodeFailure, Either[(CSV.Row, T), T]](Error.DecodeFailure.single("Unexpected End Of Input"))
          )(nel =>
            R.value.readPartial(CSV.Row(nel))
          )
        ).parMapN{
          case (h, Left((row, t))) => Either.left((row, h :: t))
          case (h, Right(t)) => Either.right(h :: t)
        }
    }
  }
}

private[internal] trait LowPriorityReadProofs{
  implicit def hlistRead2[H, T <: HList](
    implicit RH: Lazy[Read[H]],
    RT: Lazy[Read[T]]
  ): Read[H :: T] = new Read[H :: T]{
    def readPartial(a: CSV.Row): Either[Error.DecodeFailure,Either[(CSV.Row, H :: T),H :: T]] = 
      RH.value.readPartial(a).flatMap{
        case Left((row, h)) => RT.value.readPartial(row).map{
          case Left((row, t)) => Left((row, h :: t))
          case Right(t) => Right(h:: t)
        }
        case Right(value) => 
          Either.left(Error.DecodeFailure.single(s"Incomplete Output - $value only"))
      }
  }
}