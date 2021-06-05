package io.chrisdavenport.cormorant.generic

import cats.data._
import cats.implicits._
import _root_.io.chrisdavenport.cormorant._
import _root_.io.chrisdavenport.cormorant.implicits._
import _root_.io.chrisdavenport.cormorant.generic.semiauto._

class SemiAutoSpec extends munit.FunSuite {

  test("encode a write row correctly") {
    case class Example(i: Int, s: String, b: Int)
    implicit val writeExample: Write[Example] = deriveWrite

    val encoded = Encoding.writeRow(Example(1,"Hello",73))
    val expected = CSV.Row(NonEmptyList.of(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))
    
    assertEquals(encoded, expected)
  }

  test("encode a labelledWrite complete correctly") {
    case class Example(i: Int, s: Option[String], b: Int)
    implicit val writeExample: LabelledWrite[Example] = deriveLabelledWrite

    val encoded = Encoding.writeComplete(List(Example(1, Option("Hello"), 73)))
    val expected = CSV.Complete(
      CSV.Headers(NonEmptyList.of(CSV.Header("i"), CSV.Header("s"), CSV.Header("b"))),
      CSV.Rows(List(CSV.Row(NonEmptyList.of(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))))
    )
    assertEquals(encoded, expected)
  }

  test("read a correctly encoded row") {
    case class Example(i: Int, s: Option[String], b: Int)
    implicit val derivedRead: Read[Example] = deriveRead
    val from = CSV.Row(NonEmptyList.of(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))
    val expected = Example(1, Some("Hello"), 73)
    assertEquals(Read[Example].read(from), Right(expected) )
  }

  test("read a labelledRead row by name") {
    import cats.syntax.either._

    case class Example(i: Int, s: Option[String], b: Int)
    implicit val labelledReadExampled : LabelledRead[Example] = deriveLabelledRead

    // Notice That the order is different than the example above
    val fromCSV = CSV.Complete(
      CSV.Headers(NonEmptyList.of(CSV.Header("b"), CSV.Header("s"), CSV.Header("i"))),
      CSV.Rows(List(CSV.Row(NonEmptyList.of(CSV.Field("73"), CSV.Field("Hello"), CSV.Field("1")))))
    )
    val expected = List(Example(1, Option("Hello"), 73)).map(Either.right)
    
    assertEquals(Decoding.readLabelled[Example](fromCSV), expected)
  }

  test("read a product field row") {
    case class Foo(i: Int)
    case class Example(i: Foo, s: Option[String], b: Int)
    implicit val f : Read[Foo] = deriveRead
    val _ = f
    implicit val r : Read[Example] = deriveRead

    val fromCSV = 
      CSV.Row(NonEmptyList.of(CSV.Field("73"), CSV.Field("Hello"), CSV.Field("1")))

    assertEquals(r.read(fromCSV), Right(Example(Foo(73), Some("Hello"), 1)))
  }

  test("write a product field row") {
    case class Foo(i: Int, x: String)
    case class Example(i: Foo, s: Option[String], b: Int)
    implicit val f : Write[Foo] = deriveWrite
    val _ = f
    implicit val r : Write[Example] = deriveWrite
    val input = Example(Foo(73, "yellow"), Some("foo"), 5)
    assertEquals(r.write(input), CSV.Row(
      NonEmptyList.of(CSV.Field("73"), CSV.Field("yellow"), CSV.Field("foo"), CSV.Field("5"))
    ))
  }

  test("read a labelled product field row") {
    case class Foo(i: Int)
    case class Example(i: Foo, s: Option[String], b: Int)
    implicit val f : LabelledRead[Foo] = deriveLabelledRead
    val _ = f
    implicit val r : LabelledRead[Example] = deriveLabelledRead
    val fromCSV = CSV.Complete(
      CSV.Headers(NonEmptyList.of(CSV.Header("b"), CSV.Header("s"), CSV.Header("i"))),
      CSV.Rows(List(CSV.Row(NonEmptyList.of(CSV.Field("73"), CSV.Field("Hello"), CSV.Field("1")))))
    )
    val expected = List(Example(Foo(1), Option("Hello"), 73))
      .map(Either.right)
    
    assertEquals(Decoding.readLabelled[Example](fromCSV), expected)
  }

  test("write a labelled product field row") {
    case class Foo(i: Int, m: String)
    case class Example(i: Foo, s: Option[String], b: Int)
    implicit val f : LabelledWrite[Foo] = deriveLabelledWrite
    val _ = f
    implicit val w : LabelledWrite[Example] = deriveLabelledWrite
    val encoded = Encoding.writeComplete(List(Example(Foo(1, "bar"), Option("Hello"), 73)))
    val expected = CSV.Complete(
      CSV.Headers(NonEmptyList.of(CSV.Header("i"), CSV.Header("m"), CSV.Header("s"), CSV.Header("b"))),
      CSV.Rows(List(CSV.Row(NonEmptyList.of(CSV.Field("1"), CSV.Field("bar"), CSV.Field("Hello"), CSV.Field("73")))))
    )
    assertEquals(encoded, expected)
  }

}