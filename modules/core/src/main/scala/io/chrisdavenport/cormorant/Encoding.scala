package io.chrisdavenport.cormorant

object Encoding {
  def encodeWithHeaders[A: Write](xs: List[A], headers: CSV.Headers): CSV.Complete =
    CSV.Complete(headers, encodeRows(xs))
  def encodeRows[A: Write](xs: List[A]): CSV.Rows = CSV.Rows(xs.map(Write[A].write))
}