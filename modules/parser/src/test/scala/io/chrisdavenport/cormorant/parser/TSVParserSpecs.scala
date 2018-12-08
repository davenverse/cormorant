package io.chrisdavenport.cormorant.parser

import org.specs2._
import _root_.io.chrisdavenport.cormorant._
import atto._
import Atto._
import cats.implicits._
import _root_.cats.data._

class TSVParserSpec extends mutable.Specification {
  // override def is = s2"""
  // Parse a simple csv header $parseASimpleCSVHeader
  // """
  "TSVParser" should {
    "parse a single header" in {
      val basicString = "Something\t"
      val expect = CSV.Header("Something")
      TSVParser.name.parse(basicString).done must_=== ParseResult.Done("\t", expect)
    }

    "parse first header in a header list" in {
      val baseHeader = "Something\tSomething2\tSomething3"
      val expect = CSV.Header("Something")
      
      TSVParser.name.parse(baseHeader) must_=== ParseResult.Done("\tSomething2\tSomething3", expect)
    }

    "parse a group of headers" in {
      val baseHeader = "Something\tSomething2\tSomething3"
      val expect = List(
        CSV.Header("Something"),
        CSV.Header("Something2"),
        CSV.Header("Something3")
      )
      val result = (TSVParser.name, many(TSVParser.TAB ~> TSVParser.name)).mapN(_ :: _).parse(baseHeader).done
      result must_=== ParseResult.Done("", expect)
    }

    "parse headers correctly" in {
      val baseHeader = "Something\tSomething2\tSomething3"
      val expect = CSV.Headers(
        NonEmptyList.of(
          CSV.Header("Something"),
          CSV.Header("Something2"),
          CSV.Header("Something3")
        )
      )
      val result = TSVParser.header.parse(baseHeader).done

      result must_== ParseResult.Done("", expect)
    }

    "parse a row correctly" in {
      val singleRow = "yellow\tgreen\tblue"
      val expected = CSV.Row(
        NonEmptyList.of(
          CSV.Field("yellow"),
          CSV.Field("green"),
          CSV.Field("blue")
        )
      )

      TSVParser.record.parse(singleRow).done.either must_=== Right(expected)
    }

    "parse rows correctly" in {
      val csv = CSV.Rows(
        List(
          CSV.Row(NonEmptyList.of(CSV.Field("Blue"), CSV.Field("Pizza"), CSV.Field("1"))),
          CSV.Row(NonEmptyList.of(CSV.Field("Red"), CSV.Field("Margarine"), CSV.Field("2")))
        )
      )
      val csvParse = "Blue\tPizza\t1\nRed\tMargarine\t2"
      TSVParser.fileBody.parse(csvParse).done.either must_=== Either.right(csv)
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
      val expectedCSVString = 
        "Color\tFood\tNumber\nBlue\tPizza\t1\nRed\tMargarine\t2\nYellow\tBroccoli\t3"

      TSVParser.`complete-file`
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
      val expectedCSVString = 
      "Color\tFood\tNumber\nBlue\tPizza\t1\nRed\tMargarine\t2\nYellow\tBroccoli\t3\n"

      TSVParser.`complete-file`
        .parse(expectedCSVString)
        .done
        .either
        .map(_.stripTrailingRow) must_=== Either.right(csv)
    }

    "parse an escaped row with a tab" in {
      val csv = CSV.Row(NonEmptyList.of(
        CSV.Field("Green"),
        CSV.Field("Yellow\tDog"),
        CSV.Field("Blue")
      ))
      val parseString = "Green\t\"Yellow\tDog\"\tBlue"
      TSVParser.record.parse(parseString).done.either must_=== Either.right(csv)
    }

    "parse an escaped row with a double quote escaped" in {
      val csv = CSV.Row(NonEmptyList.of(
        CSV.Field("Green"),
        CSV.Field("Yellow\t \"Dog\""),
        CSV.Field("Blue")
      ))
      val parseString = "Green\t\"Yellow\t \"\"Dog\"\"\"\tBlue"
      TSVParser.record.parse(parseString).done.either must_=== Either.right(csv)
    }

    

    "parse an escaped row with embedded newline" in {
      val csv = CSV.Row(NonEmptyList.of(
        CSV.Field("Green"),
        CSV.Field("Yellow\n Dog"),
        CSV.Field("Blue")
      ))
      val parseString = "Green\t\"Yellow\n Dog\"\tBlue"
      TSVParser.record.parse(parseString).done.either must_=== Either.right(csv)
    }

    "parse an escaped row with embedded CRLF" in {
      val csv = CSV.Row(NonEmptyList.of(
        CSV.Field("Green"),
        CSV.Field("Yellow\r\n Dog"),
        CSV.Field("Blue")
      ))
      val parseString = "Green\t\"Yellow\r\n Dog\"\tBlue"
      TSVParser.record.parse(parseString).done.either must_=== Either.right(csv)
    }

  }
}