package io.chrisdavenport.cormorant.parser

import cats.implicits._
import io.chrisdavenport.cormorant._
import org.specs2._
import org.scalacheck._

class PrinterParserParity extends mutable.Specification with ScalaCheck {

  implicit val arbField : Arbitrary[CSV.Field] = Arbitrary(
    Gen.listOf(Gen.asciiChar).map(_.mkString).map(CSV.Field.apply)
  )
  implicit val arbRow: Arbitrary[CSV.Row] = Arbitrary(
    for {
      field <- Arbitrary.arbitrary[CSV.Field]
      list <- Gen.listOf(Arbitrary.arbitrary[CSV.Field])
    } yield CSV.Row(field :: list)
  )

  implicit val arbRows :Arbitrary[CSV.Rows] = Arbitrary(
    for {
      row <- Arbitrary.arbitrary[CSV.Row]
      l <- Gen.listOf(Arbitrary.arbitrary[CSV.Row])
    } yield CSV.Rows(row :: l)
  )

  implicit val arbHeader : Arbitrary[CSV.Header] = Arbitrary(
    Gen.listOf(Gen.asciiChar).map(_.mkString).map(CSV.Header.apply)
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

  "Printer should round trip with parser" in {
    "field should round trip" in  prop {  a : CSV.Field =>
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseField(encoded) must_=== Either.right(a)
    }

    "row should round trip" in prop { a: CSV.Row => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseRow(encoded) must_=== Either.right(a)
    }

    "rows should round trip" in prop { a: CSV.Rows => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseRows(encoded) must_=== Either.right(a)
    }

    "header should round trip" in prop {a: CSV.Header => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseHeader(encoded) must_=== Either.right(a)
    }

    "headers should round trip" in prop {a: CSV.Headers => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseHeaders(encoded) must_=== Either.right(a)
    }

    "complete should round trip" in prop {a: CSV.Complete => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseComplete(encoded) must_=== Either.right(a)
    }

  }

}
