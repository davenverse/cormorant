package io.chrisdavenport.cormorant

import org.specs2._
import _root_.cats.data._

object PrinterSpec extends Specification {
  override def is = s2"""
  Print a simple csv $simpleCSVPrint
  Printer field with a surrounded field $fieldSurroundedCorrectly
  Printer field with escaped field $fieldEscapedCorrectly
  """

  def simpleCSVPrint = {
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

    Printer.default.print(csv) should_=== expectedCSVString
  }


  def fieldSurroundedCorrectly = {
    val csv = CSV.Field("Snow, John")
    val expectedCSVString = "\"Snow, John\""
    
    Printer.default.print(csv) should_=== expectedCSVString
  }

  def fieldEscapedCorrectly = {
    val csv = CSV.Field("Snow, \"John\"")
    val expectedCSVString = "\"Snow, \"\"John\"\"\""
    
    Printer.default.print(csv) should_=== expectedCSVString
  }

}