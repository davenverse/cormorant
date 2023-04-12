package io.chrisdavenport.cormorant

import org.scalacheck._
import _root_.cats.data._

trait CormorantArbitraries {
  // Necessary for Round-tripping for fs2. As we can't always clarify the empty string following
  // semantics. We use printable to remove the subset that doesn't pass through utf8 encoding
  implicit val arbField: Arbitrary[CSV.Field] = Arbitrary(
    for {
      char <- Gen.asciiPrintableChar
      string <- Gen.asciiPrintableStr
    } yield CSV.Field(char.toString() + string)
  )

  // Must be 1 or More
  def genRow(s: Int): Gen[CSV.Row] = for {
    field <- Arbitrary.arbitrary[CSV.Field]
    list <- Gen.listOfN(s - 1, Arbitrary.arbitrary[CSV.Field])
  } yield CSV.Row(NonEmptyList(field, list))

  implicit val arbRow: Arbitrary[CSV.Row] = Arbitrary(
    for {
      size <- Gen.choose(1, 25)
      row <- genRow(size)
    } yield row
  )

  // Must be 1 or more
  def genRows(s: Int): Gen[CSV.Rows] = for {
    row <- genRow(s)
    l <- Gen.listOf(genRow(s))
  } yield CSV.Rows(row :: l)

  implicit val arbRows: Arbitrary[CSV.Rows] = Arbitrary(
    for {
      choose <- Gen.choose(1, 25)
      rows <- genRows(choose)
    } yield rows
  )

  // Same logic as fields
  implicit val arbHeader: Arbitrary[CSV.Header] = Arbitrary(
    for {
      char <- Gen.asciiPrintableChar
      string <- Gen.asciiPrintableStr
    } yield CSV.Header(char.toString() + string)
  )

  // Must be 1 or more
  def genHeaders(s: Int): Gen[CSV.Headers] = for {
    header <- Arbitrary.arbitrary[CSV.Header]
    list <- Gen.listOfN(s - 1, Arbitrary.arbitrary[CSV.Header])
  } yield CSV.Headers(NonEmptyList(header, list))

  implicit val arbHeaders: Arbitrary[CSV.Headers] = Arbitrary(
    for {
      choose <- Gen.choose(1, 25)
      headers <- genHeaders(choose)
    } yield headers
  )

  implicit val arbComplete: Arbitrary[CSV.Complete] = Arbitrary(
    for {
      choose <- Gen.choose(1, 25)
      headers <- genHeaders(choose)
      rows <- genRows(choose)
    } yield CSV.Complete(headers, rows)
  )

}

object CormorantArbitraries extends CormorantArbitraries
