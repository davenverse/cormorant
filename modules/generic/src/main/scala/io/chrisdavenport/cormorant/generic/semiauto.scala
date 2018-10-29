package io.chrisdavenport.cormorant.generic

import io.chrisdavenport.cormorant._
import shapeless._
import shapeless.labelled._
import cats.implicits._
import cats.data.{Validated, NonEmptyList}

object semiauto {
  implicit def hnilWrite[H](implicit P: Put[H]): Write[H :: HNil] = new Write[H :: HNil] {
    def write(a: H :: HNil): CSV.Row = CSV.Row(NonEmptyList.one(P.put(a.head)))
  }
  implicit def hlistWrite[H, T <: HList](
      implicit P: Put[H],
      W: Write[T]
  ): Write[H :: T] = new Write[H :: T] {
    def write(a: H :: T): CSV.Row = {
      val h = Put[H].put(a.head)
      val tO = Option(Write[T].write(a.tail).l)
      val lR = tO.fold(NonEmptyList.of(h))(t => NonEmptyList(h, t.toList))
      CSV.Row(lR)
    }
  }

  def deriveWrite[A, R](
      implicit gen: Generic.Aux[A, R],
      enc: Write[R]
  ): Write[A] = new Write[A] {
    def write(a: A): CSV.Row = Write[gen.Repr].write(gen.to(a))
  }

  // TODO: MUST BE A BETTER WAY TO DO THIS
  implicit def labelledWriteHNil[K <: Symbol, H](implicit witness: Witness.Aux[K],
      P: Lazy[Put[H]]): LabelledWrite[FieldType[K, H] :: HNil] = new LabelledWrite[FieldType[K, H] :: HNil] {
    def headers: CSV.Headers = CSV.Headers(NonEmptyList.one(CSV.Header(witness.value.name)))
    def write(a: FieldType[K, H] :: HNil): CSV.Row = CSV.Row(NonEmptyList.one(P.value.put(a.head)))
  }

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

  def deriveLabelledWrite[A, H <: HList](
      implicit gen: LabelledGeneric.Aux[A, H],
      hlw: Lazy[LabelledWrite[H]]): LabelledWrite[A] = new LabelledWrite[A] {
    val writeH: LabelledWrite[H] = hlw.value
    def headers: CSV.Headers = writeH.headers
    def write(a: A): CSV.Row = writeH.write(gen.to(a))
  }

  implicit def readHNil[H](implicit G: Get[H]): Read[H :: HNil] = new Read[H :: HNil] {
    def read(a: CSV.Row): Either[Error.DecodeFailure, H :: HNil] = a match {
      case CSV.Row(NonEmptyList(f, Nil)) => 
        G.get(f).map(h => h :: HNil)
      case _ => Either.left(Error.DecodeFailure.single(s"Unexpected Input: Did Not Expect - $a"))
    }
      
  }

  implicit def hlistRead[H, T <: HList](
      implicit G: Get[H],
      R: Lazy[Read[T]]
  ): Read[H :: T] = new Read[H :: T] {
    def read(a: CSV.Row): Either[Error.DecodeFailure, H :: T] = a match {
      case CSV.Row(NonEmptyList(h, t)) =>
        val myH : Validated[Error.DecodeFailure, H] = G.get(h).toValidated
        val myT : Validated[Error.DecodeFailure, T] = NonEmptyList.fromList(t)
          .fold(
            Validated.invalid[Error.DecodeFailure, T](Error.DecodeFailure.single("Unexpected End Of Input"))
          )(nel =>
            R.value.read(CSV.Row(nel)).toValidated
          )

          (myH, myT).mapN(_ :: _).toEither
    }
  }

  def deriveRead[A, R](
      implicit gen: Generic.Aux[A, R],
      R: Lazy[Read[R]]
  ): Read[A] = new Read[A] {
    def read(a: CSV.Row): Either[Error.DecodeFailure, A] =
      R.value.read(a).map(gen.from)
  }

  implicit val labelledReadHNil: LabelledRead[HNil] = new LabelledRead[HNil] {
    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure, HNil] = Either.right(HNil)
  }

  implicit def deriveLabelledReadHList[K <: Symbol, H, T <: HList](
      implicit witness: Witness.Aux[K],
      P: Lazy[Get[H]],
      labelledRead: LabelledRead[T]
  ): LabelledRead[FieldType[K, H] :: T] = new LabelledRead[FieldType[K, H] :: T] {
    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure, FieldType[K, H] :: T] = {
      implicit val getAvailable: Get[H] = P.value

      (
        Cursor.decodeAtHeader[H](CSV.Header(witness.value.name))(h, a).map(field[K](_)),
        labelledRead.read(a, h).toValidated)
        .mapN(_ :: _)
        .toEither
    }
  }

  def deriveLabelledRead[A, H <: HList](
      implicit gen: LabelledGeneric.Aux[A, H],
      hlw: Lazy[LabelledRead[H]]): LabelledRead[A] = new LabelledRead[A] {
    val readH: LabelledRead[H] = hlw.value
    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure, A] =
      readH.read(a, h).map(gen.from)
  }

}
