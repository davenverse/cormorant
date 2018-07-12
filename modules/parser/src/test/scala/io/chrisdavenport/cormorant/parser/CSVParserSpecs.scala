package io.chrisdavenport.cormorant.parser

import org.specs2._
import _root_.io.chrisdavenport.cormorant._
import atto._
import Atto._
import cats.implicits._

class CSVParserSpec extends mutable.Specification {
  // override def is = s2"""
  // Parse a simple csv header $parseASimpleCSVHeader
  // """
  "CSVParserSpec" should {
    "parse a single header" in {
      val basicString = "Something,"
      val expect = CSV.Header("Something")
      CSVParser.name.parse(basicString).done must_=== ParseResult.Done(",", expect)
    }

    "parse first header in a header list" in {
      val baseHeader = "Something,Something2,Something3"
      val expect = CSV.Header("Something")
      
      CSVParser.name.parse(baseHeader) must_=== ParseResult.Done(",Something2,Something3", expect)
      // (CSVParser.name, many(CSVParser.COMMA ~> CSVParser.name)).mapN(_ :: _).parse(baseHeader) must_=== ParseResult.Done("", expect)
    }

    "parse a group of headers" in {
      val baseHeader = "Something,Something2,Something3"
      val expect = List(
        CSV.Header("Something"),
        CSV.Header("Something2"),
        CSV.Header("Something3")
      )
      val result = (CSVParser.name, many(CSVParser.COMMA ~> CSVParser.name)).mapN(_ :: _).parse(baseHeader).done
      result must_=== ParseResult.Done("", expect)
    }

    "parse headers correctly" in {
      val baseHeader = """Something,Something2,Something3"""
      val expect = CSV.Headers(
        List(
          CSV.Header("Something"),
          CSV.Header("Something2"),
          CSV.Header("Something3")
        )
      )
      val result = CSVParser.header.parse(baseHeader).done

      result must_== ParseResult.Done("", expect)
    }

    // "complete a csv parse" in {
    //   val csv = CSV.Complete(
    //     CSV.Headers(
    //       List(CSV.Header("Color"), CSV.Header("Food"), CSV.Header("Number"))
    //     ),
    //     CSV.Rows(
    //       List(
    //         CSV.Row(List(CSV.Field("Blue"), CSV.Field("Pizza"), CSV.Field("1"))),
    //         CSV.Row(List(CSV.Field("Red"), CSV.Field("Margarine"), CSV.Field("2"))),
    //         CSV.Row(List(CSV.Field("Yellow"), CSV.Field("Broccoli"), CSV.Field("3")))
    //       )
    //     )
    //   )
    //   val expectedCSVString = """Color,Food,Number
    //   |Blue,Pizza,1
    //   |Red,Margarine,2
    //   |Yellow,Broccoli,3""".stripMargin

    //   _root_.io.chrisdavenport.cormorant.parser.csv.parseComplete(expectedCSVString) must_=== Either.right(csv)
    // } 


  }

  // def parseASingaleHeaderValueTillAComma = 

  // def parseASimpleCSVHeader = {
  //   val baseString = """Something,Something2,Something3
  //   |Value1,Value2,Value3
  //   """.stripMargin

  //   val expected = CSV.Headers(
  //     List(
  //       CSV.Header("Something"),
  //       CSV.Header("Something2"),
  //       CSV.Header("Something3")
  //     )
  //   )
  //   (CSVParser.header <~ takeRest).parse(baseString) must_== ParseResult.Done("\nValue1,Value2,Value3\n", expected)
  // }
}