package io.chrisdavenport.cormorant.generic
import org.specs2._

object SemiAutoSpec extends Specification {
  override def is = s2"""
  encode a write row correctly $rowGenericallyDerived
  encode a labelledWrite complete correctly $rowNameDerived
  """

  def rowGenericallyDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._

    case class Example(i: Int, s: String, b: Int)
    object Example{
      implicit val writeExample: Write[Example] = deriveWrite
    }
    val encoded = Encoding.encodeRow(Example(1,"Hello",73))
    val expected = CSV.Row(List(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))
    
    encoded must_=== expected
  }

  def rowNameDerived = {
    import _root_.io.chrisdavenport.cormorant._
    import _root_.io.chrisdavenport.cormorant.implicits._
    import _root_.io.chrisdavenport.cormorant.generic.semiauto._
    case class Example(i: Int, s: Option[String], b: Int)
    object Example{
      implicit val writeExample: LabelledWrite[Example] = deriveLabelledWrite
    }
    val encoded = Encoding.encode(List(Example(1, Option("Hello"), 73)))
    val expected = CSV.Complete(
      CSV.Headers(List(CSV.Header("i"), CSV.Header("s"), CSV.Header("b"))),
      CSV.Rows(List(CSV.Row(List(CSV.Field("1"), CSV.Field("Hello"), CSV.Field("73")))))
    )
    encoded must_=== expected
  }
}