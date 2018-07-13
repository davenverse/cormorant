package io.chrisdavenport.cormorant

object Encoding {
  def writeWithHeaders[A: Write](xs: List[A], headers: CSV.Headers): CSV.Complete =
    CSV.Complete(headers, writeRows(xs))

  def writeRow[A: Write](a: A): CSV.Row = Write[A].write(a)
  def writeRows[A: Write](xs: List[A]): CSV.Rows = CSV.Rows(xs.map(Write[A].write))

  def writeComplete[A: LabelledWrite](xs: List[A]): CSV.Complete =
    CSV.Complete(LabelledWrite[A].headers, CSV.Rows(xs.map(LabelledWrite[A].write)))
}
