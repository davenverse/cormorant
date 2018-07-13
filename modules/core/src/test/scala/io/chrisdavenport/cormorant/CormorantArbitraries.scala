package io.chrisdavenport.cormorant

import org.scalacheck._

trait CormorantArbitraries {
    implicit val arbField : Arbitrary[CSV.Field] = Arbitrary(
    Gen.listOf(Arbitrary.arbitrary[String]).map(_.mkString).map(CSV.Field.apply)
  )
  implicit val arbRow: Arbitrary[CSV.Row] = Arbitrary(
    for {
      field <- Arbitrary.arbitrary[CSV.Field]
      list <- Gen.listOf(Arbitrary.arbitrary[CSV.Field])
    } yield CSV.Row(field :: list)
  )

  implicit val arbRows : Arbitrary[CSV.Rows] = Arbitrary(
    for {
      row <- Arbitrary.arbitrary[CSV.Row]
      l <- Gen.listOf(Arbitrary.arbitrary[CSV.Row])
    } yield CSV.Rows(row :: l)
  )

  implicit val arbHeader : Arbitrary[CSV.Header] = Arbitrary(
    Gen.listOf(Arbitrary.arbitrary[String]).map(_.mkString).map(CSV.Header.apply)
  )

  implicit val arbHeaders : Arbitrary[CSV.Headers] = Arbitrary(
    for {
      header <- Arbitrary.arbitrary[CSV.Header]
      list <- Gen.listOf(Arbitrary.arbitrary[CSV.Header])
    } yield CSV.Headers(header :: list)
  )

  implicit val arbComplete : Arbitrary[CSV.Complete] = Arbitrary(
    for {
      headers <- Arbitrary.arbitrary[CSV.Headers]
      rows <- Arbitrary.arbitrary[CSV.Rows]
    } yield CSV.Complete(headers, rows)
  )

}

object CormorantArbitraries extends CormorantArbitraries