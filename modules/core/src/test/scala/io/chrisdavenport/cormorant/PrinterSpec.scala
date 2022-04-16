package io.chrisdavenport.cormorant

import _root_.cats.data._

class PrinterSpec extends munit.FunSuite {

  test("Print a simple csv") {
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

    assertEquals(Printer.default.print(csv), expectedCSVString)
  }

  test("Printer field with a surrounded field") {
    val csv = CSV.Field("Snow, John")
    val expectedCSVString = "\"Snow, John\""

    assertEquals(Printer.default.print(csv), expectedCSVString)
  }

  test("Printer field with escaped field") {
    val csv = CSV.Field("Snow, \"John\"")
    val expectedCSVString = "\"Snow, \"\"John\"\"\""

    assertEquals(Printer.default.print(csv), expectedCSVString)
  }
}
