package io.chrisdavenport.cormorant.parser

import cats.implicits._
import io.chrisdavenport.cormorant._
import org.specs2._
import org.scalacheck._

class PrinterParserParity extends mutable.Specification with ScalaCheck {

  implicit val arbField : Arbitrary[CSV.Field] = Arbitrary(
    Gen.listOf(Gen.asciiChar).map(_.mkString).map(_.replace("\"", "")).map(CSV.Field.apply)
  )
  implicit val arbRow: Arbitrary[CSV.Row] = Arbitrary(
    for {
      field <- Arbitrary.arbitrary[CSV.Field]
      list <- Gen.listOf(Arbitrary.arbitrary[CSV.Field])
    } yield CSV.Row(field :: list)
  )

  "Printer should round trip with parser" in {
    "field should round trip" in  prop {  field : CSV.Field =>
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = field.print(Printer.default)
      parseField(encoded) must_=== Either.right(field)
    }

    "row should round trip" in prop { row: CSV.Row => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = row.print(Printer.default)
      parseRow(encoded) must_=== Either.right(row)
    }

  }

}
