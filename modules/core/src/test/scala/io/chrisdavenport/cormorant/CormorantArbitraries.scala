package io.chrisdavenport.cormorant

import org.scalacheck._
import _root_.cats.data._

trait CormorantArbitraries {
    implicit val arbField : Arbitrary[CSV.Field] = Arbitrary(
    Gen.listOf(Arbitrary.arbitrary[String]).map(_.mkString).map(CSV.Field.apply)
  )

  // Must be 1 or More
  def genRow(s: Int): Gen[CSV.Row] = for {
      field <- Arbitrary.arbitrary[CSV.Field]
      list <- Gen.listOfN(s - 1, Arbitrary.arbitrary[CSV.Field])
    } yield CSV.Row(NonEmptyList(field, list))

  implicit val arbRow: Arbitrary[CSV.Row] = Arbitrary(
    for {
      size <- Gen.choose(1, 100)
      row <- genRow(size)
    } yield row
  )

  // Must be 1 or more
  def genRows(s: Int): Gen[CSV.Rows] = for {
      row <- genRow(s)
      l <- Gen.listOf(genRow(s))
    } yield CSV.Rows(row :: l)

  implicit val arbRows : Arbitrary[CSV.Rows] = Arbitrary(
    for {
      choose <- Gen.choose(1, 100)
      rows <- genRows(choose)
    } yield rows
  )

  implicit val arbHeader : Arbitrary[CSV.Header] = Arbitrary(
    Gen.listOf(Arbitrary.arbitrary[String]).map(_.mkString).map(CSV.Header.apply)
  )

  // Must be 1 or more
  def genHeaders(s: Int): Gen[CSV.Headers] = for {
      header <- Arbitrary.arbitrary[CSV.Header]
      list <- Gen.listOfN(s- 1, Arbitrary.arbitrary[CSV.Header])
  } yield CSV.Headers(NonEmptyList(header,list))

  implicit val arbHeaders : Arbitrary[CSV.Headers] = Arbitrary(
    for {
      choose <- Gen.choose(1, 100)
      headers <- genHeaders(choose)
    } yield headers
  )

  implicit val arbComplete : Arbitrary[CSV.Complete] = Arbitrary(
    for {
      choose <- Gen.choose(1, 100)
      headers <- genHeaders(choose)
      rows <- genRows(choose)
    } yield CSV.Complete(headers, rows)
  )

}

object CormorantArbitraries extends CormorantArbitraries