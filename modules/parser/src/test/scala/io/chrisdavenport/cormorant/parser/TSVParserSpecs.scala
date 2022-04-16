package io.chrisdavenport.cormorant.parser

import _root_.io.chrisdavenport.cormorant._
import atto._
import Atto._
import cats.syntax.all._
import _root_.cats.data._

class TSVParserSpec extends munit.FunSuite {

  test("parse a single header") {
    val basicString = "Something\t"
    val expect = CSV.Header("Something")
    assertEquals(TSVParser.name.parse(basicString).done, ParseResult.Done("\t", expect))
  }

  test("parse first header in a header list") {
    val baseHeader = "Something\tSomething2\tSomething3"
    val expect = CSV.Header("Something")

    assertEquals(TSVParser.name.parse(baseHeader), ParseResult.Done("\tSomething2\tSomething3", expect))
  }

  test("parse a group of headers") {
    val baseHeader = "Something\tSomething2\tSomething3"
    val expect = List(
      CSV.Header("Something"),
      CSV.Header("Something2"),
      CSV.Header("Something3")
    )
    val result = (TSVParser.name, many(TSVParser.SEPARATOR ~> TSVParser.name)).mapN(_ :: _).parse(baseHeader).done
    assertEquals(result, ParseResult.Done("", expect))
  }

  test("parse headers correctly") {
    val baseHeader = "Something\tSomething2\tSomething3"
    val expect = CSV.Headers(
      NonEmptyList.of(
        CSV.Header("Something"),
        CSV.Header("Something2"),
        CSV.Header("Something3")
      )
    )
    val result = TSVParser.header.parse(baseHeader).done

    assertEquals(result, ParseResult.Done("", expect))
  }

  test("parse a row correctly") {
    val singleRow = "yellow\tgreen\tblue"
    val expected = CSV.Row(
      NonEmptyList.of(
        CSV.Field("yellow"),
        CSV.Field("green"),
        CSV.Field("blue")
      )
    )

    assertEquals(TSVParser.record.parse(singleRow).done.either, Right(expected))
  }

  test("parse rows correctly") {
    val csv = CSV.Rows(
      List(
        CSV.Row(NonEmptyList.of(CSV.Field("Blue"), CSV.Field("Pizza"), CSV.Field("1"))),
        CSV.Row(NonEmptyList.of(CSV.Field("Red"), CSV.Field("Margarine"), CSV.Field("2")))
      )
    )
    val csvParse = "Blue\tPizza\t1\nRed\tMargarine\t2"
    assertEquals(TSVParser.fileBody.parse(csvParse).done.either, Either.right(csv))
  }

  test("complete a csv parse") {
    val csv = CSV.Complete(
      CSV.Headers(
        NonEmptyList.of(CSV.Header("Color"), CSV.Header("Food"), CSV.Header("Number"))
      ),
      CSV.Rows(
        List(
          CSV.Row(NonEmptyList.of(CSV.Field("Blue"), CSV.Field("Pizza"), CSV.Field("1"))),
          CSV.Row(NonEmptyList.of(CSV.Field("Red"), CSV.Field("Margarine"), CSV.Field("2"))),
          CSV.Row(NonEmptyList.of(CSV.Field("Yellow"), CSV.Field("Broccoli"), CSV.Field("3")))
        )
      )
    )
    val expectedCSVString =
      "Color\tFood\tNumber\nBlue\tPizza\t1\nRed\tMargarine\t2\nYellow\tBroccoli\t3"

    assertEquals(TSVParser.`complete-file`
      .parse(expectedCSVString)
      .done
      .either, Either.right(csv))
  }

  test("parse a complete csv with a trailing new line by stripping it") {
    val csv = CSV.Complete(
      CSV.Headers(
        NonEmptyList.of(CSV.Header("Color"), CSV.Header("Food"), CSV.Header("Number"))
      ),
      CSV.Rows(
        List(
          CSV.Row(NonEmptyList.of(CSV.Field("Blue"), CSV.Field("Pizza"), CSV.Field("1"))),
          CSV.Row(NonEmptyList.of(CSV.Field("Red"), CSV.Field("Margarine"), CSV.Field("2"))),
          CSV.Row(NonEmptyList.of(CSV.Field("Yellow"), CSV.Field("Broccoli"), CSV.Field("3")))
        )
      )
    )
    val expectedCSVString =
      "Color\tFood\tNumber\nBlue\tPizza\t1\nRed\tMargarine\t2\nYellow\tBroccoli\t3\n"

    assertEquals(TSVParser.`complete-file`
      .parse(expectedCSVString)
      .done
      .either
      .map(_.stripTrailingRow) , Either.right(csv))
  }

  test("parse an escaped row with a tab") {
    val csv = CSV.Row(NonEmptyList.of(
      CSV.Field("Green"),
      CSV.Field("Yellow\tDog"),
      CSV.Field("Blue")
    ))
    val parseString = "Green\t\"Yellow\tDog\"\tBlue"
    assertEquals(TSVParser.record.parse(parseString).done.either, Either.right(csv))
  }

  test("parse an escaped row with a double quote escaped") {
    val csv = CSV.Row(NonEmptyList.of(
      CSV.Field("Green"),
      CSV.Field("Yellow\t \"Dog\""),
      CSV.Field("Blue")
    ))
    val parseString = "Green\t\"Yellow\t \"\"Dog\"\"\"\tBlue"
    assertEquals(TSVParser.record.parse(parseString).done.either, Either.right(csv))
  }



  test("parse an escaped row with embedded newline") {
    val csv = CSV.Row(NonEmptyList.of(
      CSV.Field("Green"),
      CSV.Field("Yellow\n Dog"),
      CSV.Field("Blue")
    ))
    val parseString = "Green\t\"Yellow\n Dog\"\tBlue"
    assertEquals(TSVParser.record.parse(parseString).done.either, Either.right(csv))
  }

  test("parse an escaped row with embedded CRLF") {
    val csv = CSV.Row(NonEmptyList.of(
      CSV.Field("Green"),
      CSV.Field("Yellow\r\n Dog"),
      CSV.Field("Blue")
    ))
    val parseString = "Green\t\"Yellow\r\n Dog\"\tBlue"
    assertEquals(TSVParser.record.parse(parseString).done.either, Either.right(csv))
  }

}