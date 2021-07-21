package io.chrisdavenport.cormorant.catsparse

import _root_.io.chrisdavenport.cormorant._
import _root_.io.chrisdavenport.cormorant.catsparse.parser._
import cats.implicits._
import _root_.cats.data._

class CSVParserSpec extends munit.FunSuite {

  test("parse a single header") {
    val basicString = "Something,"
    val expect = CSV.Header("Something")
    assertEquals(CSVParser.name.parse(basicString), Right((",", expect)))
  }

  test("parse first header in a header list") {
    val baseHeader = "Something,Something2,Something3"
    val expect = CSV.Header("Something")

    assertEquals(CSVParser.name.parse(baseHeader), Right((",Something2,Something3", expect)))
  }

  test("parse a group of headers") {
    val baseHeader = "Something,Something2,Something3"
    val expect = List(
      CSV.Header("Something"),
      CSV.Header("Something2"),
      CSV.Header("Something3")
    )
    val result = (CSVParser.name, (CSVParser.SEPARATOR *> CSVParser.name).rep0).mapN(_ :: _).parse(baseHeader)
    assertEquals(result, Right(("", expect)))
  }

  test("parse headers correctly") {
    val baseHeader = """Something,Something2,Something3"""
    val expect = CSV.Headers(
      NonEmptyList.of(
        CSV.Header("Something"),
        CSV.Header("Something2"),
        CSV.Header("Something3")
      )
    )
    val result = CSVParser.header.parse(baseHeader)

    assertEquals(result, Right(("", expect)))
  }

  test("parse a row correctly") {
    val singleRow = "yellow,green,blue"
    val expected = CSV.Row(
      NonEmptyList.of(
        CSV.Field("yellow"),
        CSV.Field("green"),
        CSV.Field("blue")
      )
    )

    assertEquals(CSVParser.record.parse(singleRow), Right(("", expected)))
  }

  test("parse rows correctly") {
    val csv = CSV.Rows(
      List(
        CSV.Row(NonEmptyList.of(CSV.Field("Blue"), CSV.Field("Pizza"), CSV.Field("1"))),
        CSV.Row(NonEmptyList.of(CSV.Field("Red"), CSV.Field("Margarine"), CSV.Field("2")))
      )
    )
    val csvParse = """Blue,Pizza,1
    |Red,Margarine,2""".stripMargin
    assertEquals(CSVParser.fileBody.parse(csvParse), Right(("", csv)))
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
    val expectedCSVString = """Color,Food,Number
    |Blue,Pizza,1
    |Red,Margarine,2
    |Yellow,Broccoli,3""".stripMargin

    assertEquals(CSVParser.`complete-file`
      .parse(expectedCSVString),
      Right(("", csv)))
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
    val expectedCSVString = """Color,Food,Number
    |Blue,Pizza,1
    |Red,Margarine,2
    |Yellow,Broccoli,3
    |""".stripMargin

    val actual = CSVParser.`complete-file`
      .parse(expectedCSVString)
      .map(_._2.stripTrailingRow)
    assertEquals(actual, Right((csv)))
  }

  test("parse an escaped row with a comma") {
    val csv = CSV.Row(NonEmptyList.of(
      CSV.Field("Green"),
      CSV.Field("Yellow,Dog"),
      CSV.Field("Blue")
    ))
    val parseString = "Green,\"Yellow,Dog\",Blue"
    assertEquals(CSVParser.record.parse(parseString), Right(("", csv)))
  }

  test("parse an escaped row with a double quote escaped") {
    val csv = CSV.Row(NonEmptyList.of(
      CSV.Field("Green"),
      CSV.Field("Yellow, \"Dog\""),
      CSV.Field("Blue")
    ))
    val parseString = "Green,\"Yellow, \"\"Dog\"\"\",Blue"
    assertEquals(CSVParser.record.parse(parseString), Right(("", csv)))
  }



  test("parse an escaped row with embedded newline") {
    val csv = CSV.Row(NonEmptyList.of(
      CSV.Field("Green"),
      CSV.Field("Yellow\n Dog"),
      CSV.Field("Blue")
    ))
    val parseString = "Green,\"Yellow\n Dog\",Blue"
    assertEquals(CSVParser.record.parse(parseString), Right(("", csv)))
  }

  test("parse an escaped row with embedded CRLF") {
    val csv = CSV.Row(NonEmptyList.of(
      CSV.Field("Green"),
      CSV.Field("Yellow\r\n Dog"),
      CSV.Field("Blue")
    ))
    val parseString = "Green,\"Yellow\r\n Dog\",Blue"
    assertEquals(CSVParser.record.parse(parseString), Right(("", csv)))
  }

}