package io.chrisdavenport.cormorant.instances

import cats.implicits._
import io.chrisdavenport.cormorant._
import scala.util.Try

trait base {
  implicit val stringGet: Get[String] = Get.by(_.x)
  implicit val stringPut: Put[String] = Put.by(CSV.Field(_))

  implicit val unitGet : Get[Unit] = new Get[Unit]{
    def get(csv: CSV.Field): Either[Error.DecodeFailure, Unit] = 
      if (csv.x == "") Right(())
      else Left(Error.DecodeFailure.single("Failed to decode Unit: Received Field $field"))
  }
  implicit val unitPut : Put[Unit] = stringPut.contramap(_ => "")

  implicit val intGet: Get[Int] = Get.tryOrMessage[Int](
    field => Try(field.x.toInt), 
    field => s"Failed to decode Int: Received Field $field"
  )
  implicit val intPut: Put[Int] = stringPut.contramap(_.toString)

  implicit val doubleGet: Get[Double] = Get.tryOrMessage[Double](
    field => Try(field.x.toDouble),
    field => s"Failed to decode Double: Received Field $field"
  )
  implicit val doublePut: Put[Double] = stringPut.contramap(_.toString)

  implicit def optionGet[A: Get]: Get[Option[A]] = new Get[Option[A]]{
    def get(field: CSV.Field): Either[Error.DecodeFailure, Option[A]] = 
      if (field.x == "") Right(Option.empty[A])
      else Get[A].map[Option[A]](a => Some(a)).get(field)
  }
  implicit def optionPut[A](implicit P: Put[A]): Put[Option[A]] = new Put[Option[A]]{
    def put(a: Option[A]): CSV.Field = a.fold(CSV.Field(""))(a => P.put(a))
  }




}