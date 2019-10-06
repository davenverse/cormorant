package io.chrisdavenport.cormorant.parser

import org.specs2._
import _root_.io.chrisdavenport.cormorant._
import atto._
import Atto._
import cats.implicits._
import _root_.cats.data._

class CSVParserSpec extends mutable.Specification {
  // override def is = s2"""
  // Parse a simple csv header $parseASimpleCSVHeader
  // """
  "CSVParser" should {
    "parse a single header" in {
      val basicString = "Something,"
      val expect = CSV.Header("Something")
      CSVParser.name.parse(basicString).done must_=== ParseResult.Done(",", expect)
    }

    "parse first header in a header list" in {
      val baseHeader = "Something,Something2,Something3"
      val expect = CSV.Header("Something")
      
      CSVParser.name.parse(baseHeader) must_=== ParseResult.Done(",Something2,Something3", expect)
    }

    "parse a group of headers" in {
      val baseHeader = "Something,Something2,Something3"
      val expect = List(
        CSV.Header("Something"),
        CSV.Header("Something2"),
        CSV.Header("Something3")
      )
      val result = (CSVParser.name, many(CSVParser.SEPARATOR ~> CSVParser.name)).mapN(_ :: _).parse(baseHeader).done
      result must_=== ParseResult.Done("", expect)
    }

    "parse headers correctly" in {
      val baseHeader = """Something,Something2,Something3"""
      val expect = CSV.Headers(
        NonEmptyList.of(
          CSV.Header("Something"),
          CSV.Header("Something2"),
          CSV.Header("Something3")
        )
      )
      val result = CSVParser.header.parse(baseHeader).done

      result must_== ParseResult.Done("", expect)
    }

    "parse a row correctly" in {
      val singleRow = "yellow,green,blue"
      val expected = CSV.Row(
        NonEmptyList.of(
          CSV.Field("yellow"),
          CSV.Field("green"),
          CSV.Field("blue")
        )
      )

      CSVParser.record.parse(singleRow).done.either must_=== Right(expected)
    }

    "parse rows correctly" in {
      val csv = CSV.Rows(
        List(
          CSV.Row(NonEmptyList.of(CSV.Field("Blue"), CSV.Field("Pizza"), CSV.Field("1"))),
          CSV.Row(NonEmptyList.of(CSV.Field("Red"), CSV.Field("Margarine"), CSV.Field("2")))
        )
      )
      val csvParse = """Blue,Pizza,1
      |Red,Margarine,2""".stripMargin
      CSVParser.fileBody.parse(csvParse).done.either must_=== Either.right(csv)
    }

    "complete a csv parse" in {
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

      CSVParser.`complete-file`
        .parse(expectedCSVString)
        .done
        .either must_=== Either.right(csv)
    }

    "parse a complete csv with a trailing new line by stripping it" in {
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

      CSVParser.`complete-file`
        .parse(expectedCSVString)
        .done
        .either
        .map(_.stripTrailingRow) must_=== Either.right(csv)
    }

    "parse an escaped row with a comma" in {
      val csv = CSV.Row(NonEmptyList.of(
        CSV.Field("Green"),
        CSV.Field("Yellow,Dog"),
        CSV.Field("Blue")
      ))
      val parseString = "Green,\"Yellow,Dog\",Blue"
      CSVParser.record.parse(parseString).done.either must_=== Either.right(csv)
    }

    "parse an escaped row with a double quote escaped" in {
      val csv = CSV.Row(NonEmptyList.of(
        CSV.Field("Green"),
        CSV.Field("Yellow, \"Dog\""),
        CSV.Field("Blue")
      ))
      val parseString = "Green,\"Yellow, \"\"Dog\"\"\",Blue"
      CSVParser.record.parse(parseString).done.either must_=== Either.right(csv)
    }

    

    "parse an escaped row with embedded newline" in {
      val csv = CSV.Row(NonEmptyList.of(
        CSV.Field("Green"),
        CSV.Field("Yellow\n Dog"),
        CSV.Field("Blue")
      ))
      val parseString = "Green,\"Yellow\n Dog\",Blue"
      CSVParser.record.parse(parseString).done.either must_=== Either.right(csv)
    }

    "parse an escaped row with embedded CRLF" in {
      val csv = CSV.Row(NonEmptyList.of(
        CSV.Field("Green"),
        CSV.Field("Yellow\r\n Dog"),
        CSV.Field("Blue")
      ))
      val parseString = "Green,\"Yellow\r\n Dog\",Blue"
      CSVParser.record.parse(parseString).done.either must_=== Either.right(csv)
    }

  }
}