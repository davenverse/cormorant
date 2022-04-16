package io.chrisdavenport.cormorant.parser

import cats.syntax.all._
import io.chrisdavenport.cormorant._
import _root_.io.chrisdavenport.cormorant.implicits._
import munit.ScalaCheckSuite
import org.scalacheck.Test.Parameters
import org.scalacheck.Prop._

class PrinterParserParity extends ScalaCheckSuite with CormorantArbitraries {

  val minTestsOK = Parameters.default
    .withMinSuccessfulTests(20)
    .withWorkers(2)

  property("field should round trip") {
    forAll { a: CSV.Field =>
      val encoded = a.print(Printer.default)
      assertEquals(parseField(encoded), Either.right(a))
    }
  }.check(minTestsOK)

  property("row should round trip") {
    forAll { a: CSV.Row =>
      val encoded = a.print(Printer.default)
      assertEquals(parseRow(encoded), Either.right(a))
    }
  }.check(minTestsOK)

  property("rows should round trip") {
    forAll { a: CSV.Rows =>
      val encoded = a.print(Printer.default)
      assertEquals(parseRows(encoded), Either.right(a))
    }
  }.check(minTestsOK)

  property("header should round trip") {
    forAll { a: CSV.Header =>
      val encoded = a.print(Printer.default)
      assertEquals(parseHeader(encoded), Either.right(a))
    }
  }.check(minTestsOK)

  property("headers should round trip") {
    forAll { a: CSV.Headers =>
      val encoded = a.print(Printer.default)
      assertEquals(parseHeaders(encoded), Either.right(a))
    }
  }.check(minTestsOK)

  property("complete should round trip") {
    forAll { a: CSV.Complete =>
      val encoded = a.print(Printer.default)
      assertEquals(parseComplete(encoded), Either.right(a))
    }
  }.check(minTestsOK)

}
