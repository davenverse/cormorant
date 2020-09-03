package io.chrisdavenport.cormorant.generic

import cats.data._
import org.specs2._
import cats.syntax.all._

class SemiAutoSpec extends Specification {
  override def is = s2"""
  encode a write row correctly $rowGenericallyDerived
  encode a labelledWrite complete correctly $rowNameDerived
  read a correctly encoded row $readRowDerived
  read a labelledRead row by name $nameBasedReadDerived
  read a product field row $derivedProductRead
  write a product field row $derivedProductWrite
  """

  def rowGenericallyDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._

    case class Example(i: Int, s: String, b: Int)
    implicit val writeExample: Write[Example] = deriveWrite

    val encoded = Encoding.writeRow(Example(1,"Hello",73))
    val expected = CSV.Row(NonEmptyList.of(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))
    
    encoded must_=== expected
  }

  def rowNameDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._
    case class Example(i: Int, s: Option[String], b: Int)
    implicit val writeExample: LabelledWrite[Example] = deriveLabelledWrite

    val encoded = Encoding.writeComplete(List(Example(1, Option("Hello"), 73)))
    val expected = CSV.Complete(
      CSV.Headers(NonEmptyList.of(CSV.Header("i"), CSV.Header("s"), CSV.Header("b"))),
      CSV.Rows(List(CSV.Row(NonEmptyList.of(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))))
    )
    encoded must_=== expected
  }

  def readRowDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._
    case class Example(i: Int, s: Option[String], b: Int)
    implicit val derivedRead: Read[Example] = deriveRead
    val from = CSV.Row(NonEmptyList.of(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))
    val expected = Example(1, Some("Hello"), 73)
    Read[Example].read(from) must_=== Right(expected) 
  }

  def nameBasedReadDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._
    import cats.syntax.either._

    case class Example(i: Int, s: Option[String], b: Int)
    implicit val labelledReadExampled : LabelledRead[Example] = deriveLabelledRead

    // Notice That the order is different than the example above
    val fromCSV = CSV.Complete(
      CSV.Headers(NonEmptyList.of(CSV.Header("b"), CSV.Header("s"), CSV.Header("i"))),
      CSV.Rows(List(CSV.Row(NonEmptyList.of(CSV.Field("73"), CSV.Field("Hello"), CSV.Field("1")))))
    )
    val expected = List(Example(1, Option("Hello"), 73)).map(Either.right)
    
    Decoding.readLabelled[Example](fromCSV) must_=== expected
  }

  def derivedProductRead = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._
    case class Foo(i: Int)
    case class Example(i: Foo, s: Option[String], b: Int)
    implicit val f : Read[Foo] = deriveRead
    val _ = f
    implicit val r : Read[Example] = deriveRead

    val fromCSV = 
      CSV.Row(NonEmptyList.of(CSV.Field("73"), CSV.Field("Hello"), CSV.Field("1")))

    r.read(fromCSV) must_=== Right(Example(Foo(73), Some("Hello"), 1))
  }

  def derivedProductWrite = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._
    case class Foo(i: Int, x: String)
    case class Example(i: Foo, s: Option[String], b: Int)
    implicit val f : Write[Foo] = deriveWrite
    val _ = f
    implicit val r : Write[Example] = deriveWrite
    val input = Example(Foo(73, "yellow"), Some("foo"), 5)
    r.write(input) must_=== CSV.Row(
      NonEmptyList.of(CSV.Field("73"), CSV.Field("yellow"), CSV.Field("foo"), CSV.Field("5"))
    )
  }

  def derivedProductLabelledRead = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._
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
    
    Decoding.readLabelled[Example](fromCSV) must_=== expected
  }

  def derivedProductLabelledWrite = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._
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
    encoded must_=== expected
  }

}