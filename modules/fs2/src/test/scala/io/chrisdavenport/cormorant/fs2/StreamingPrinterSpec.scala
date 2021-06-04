package io.chrisdavenport.cormorant
package fs2

import cats.data.NonEmptyList
import cats.effect._
import _root_.fs2.Stream
import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.implicits._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF

class StreamingPrinterSuite
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with CormorantArbitraries {

  test("Streaming printer row should round trip") {
    PropF.forAllF { (a: CSV.Row) =>
      Stream
        .emit[IO, CSV.Row](a)
        .through(encodeRows(Printer.default))
        .through(parseRows)
        .compile
        .toList
        .map(r => assertEquals(r, List(a)))
    }
  }

  test("Streaming printer rows should round trip") {
    PropF.forAllF { (a: CSV.Rows) =>
      Stream
        .emits[IO, CSV.Row](a.rows)
        .through(encodeRows(Printer.default))
        .through(parseRows)
        .compile
        .toList
        .map(CSV.Rows)
        .map(r => assertEquals(r, a))
    }
  }

  test("Streaming printer rows special case for empty removal") {
    import CSV._

    val rows = Rows(
      List(
        Row(NonEmptyList.of(Field("")))
        // Row(NonEmptyList.of(Field("")))
      )
    )
    val expected = List.empty[CSV.Row]

    Stream
      .emits[IO, CSV.Row](rows.rows)
      .through(encodeRows(Printer.default))
      .through(parseRows)
      .compile
      .toList
      .map(assertEquals(_, expected))
  }

  test("Streaming printer should complete should write as expected") {
    final case class Foo(color: String, food: String, number: Int)

    val list = List(
      Foo("Blue", "Pizza", 1),
      Foo("Red", "Margarine", 2),
      Foo("Yellow", "Broccoli", 3)
    )

    implicit val L: LabelledWrite[Foo] = new LabelledWrite[Foo] {
      override def headers: CSV.Headers =
        CSV.Headers(
          NonEmptyList.of(CSV.Header("Color"), CSV.Header("Food"), CSV.Header("Number"))
        )

      override def write(a: Foo): CSV.Row =
        CSV.Row(
          NonEmptyList.of(a.color.field, a.food.field, a.number.field)
        )
    }

    val result = Stream
      .emits(list)
      .through(writeLabelled(Printer.default))
      .compile
      .string

    val expectedCSVString = """Color,Food,Number
                                |Blue,Pizza,1
                                |Red,Margarine,2
                                |Yellow,Broccoli,3""".stripMargin

    assertEquals(result, expectedCSVString)
  }

  test("Streaming printer should round trip with streaming encoder") {
    PropF.forAllF { (csv: CSV.Complete) =>
      val expected = csv.rows.rows.map(row => (csv.headers, row))
      Stream
        .emits(csv.rows.rows)
        .through(encodeWithHeaders(csv.headers, Printer.default))
        .covary[IO]
        .through(parseComplete)
        .compile
        .toList
        .map(assertEquals(_, expected))
    }
  }

  test("Streaming printer should round trip with printer") {
    PropF.forAllF { (csv: CSV.Complete) =>
      val output = Printer.default.print(csv)
      val expected = csv.rows.rows.map(row => (csv.headers, row))
      Stream(output)
        .covary[IO]
        .through(parseComplete)
        .compile
        .toList
        .map(assertEquals(_, expected))
    }
  }
}
