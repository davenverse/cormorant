package io.chrisdavenport.cormorant

import org.specs2._

object PrinterSpec extends Specification {
  override def is = s2"""
  Print a simple csv $simpleCSVPrint
  """

  def simpleCSVPrint = {
    val csv = CSV.Complete(
      CSV.Headers(
        List(CSV.Header("Color"), CSV.Header("Food"), CSV.Header("Number"))
      ),
      CSV.Rows(
        List(
          CSV.Row(List(CSV.Field("Blue"), CSV.Field("Pizza"), CSV.Field("1"))),
          CSV.Row(List(CSV.Field("Red"), CSV.Field("Margarine"), CSV.Field("2"))),
          CSV.Row(List(CSV.Field("Yellow"), CSV.Field("Broccoli"), CSV.Field("3")))
        )
      )
    )
    val expectedCSVString = """Color,Food,Number
    |Blue,Pizza,1
    |Red,Margarine,2
    |Yellow,Broccoli,3""".stripMargin

    Printer.default.print(csv) should_=== expectedCSVString
  }


}