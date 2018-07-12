package io.chrisdavenport.cormorant.generic
import org.specs2._

class SemiAutoSpec extends Specification {
  override def is = s2"""
  encode a write row correctly $rowGenericallyDerived
  encode a labelledWrite complete correctly $rowNameDerived
  read a correctly encoded row $readRowDerived
  """

  def rowGenericallyDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._

    case class Example(i: Int, s: String, b: Int)
    implicit val writeExample: Write[Example] = deriveWrite

    val encoded = Encoding.writeRow(Example(1,"Hello",73))
    val expected = CSV.Row(List(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))
    
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
      CSV.Headers(List(CSV.Header("i"), CSV.Header("s"), CSV.Header("b"))),
      CSV.Rows(List(CSV.Row(List(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))))
    )
    encoded must_=== expected
  }

  def readRowDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._
    case class Example(i: Int, s: Option[String], b: Int)
    implicit val derivedRead: Read[Example] = deriveRead
    val from = CSV.Row(List(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))
    val expected = Example(1, Some("Hello"), 73)
    Read[Example].read(from) must_=== Right(expected) 
  }
}