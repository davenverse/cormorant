package io.chrisdavenport.cormorant.generic
import org.specs2._

class AutoSpec extends Specification {
  override def is = s2"""
  encode a row with Write automatically $rowGenericallyDerived
  encode a comple with LabelledWrite automatically $rowNameDerived
  read a row with read automatically $readRowDerived
  """

  def rowGenericallyDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.auto._

    case class Example(i: Int, s: String, b: Int)

    val encoded = Example(1,"Hello",73).writeRow
    val expected = CSV.Row(List(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))
    
    encoded must_=== expected
  }

  def rowNameDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.auto._
    case class Example(i: Int, s: Option[String], b: Int)

    val encoded = List(Example(1, Option("Hello"), 73)).writeComplete
    val expected = CSV.Complete(
      CSV.Headers(List(CSV.Header("i"), CSV.Header("s"), CSV.Header("b"))),
      CSV.Rows(List(CSV.Row(List(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))))
    )
    encoded must_=== expected
  }

  def readRowDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.auto._
    case class Example(i: Int, s: Option[String], b: Int)
    val from = CSV.Row(List(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))
    val expected = Example(1, Some("Hello"), 73)
    from.read[Example] must_=== Right(expected) 
  }


}