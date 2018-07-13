package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant._

trait write {
  implicit class writeOps[A: Write](a: A) {
    def writeRow: CSV.Row = Write[A].write(a)
  }

  implicit class writeListOps[A: Write](xs: List[A]) {
    def writeRows: CSV.Rows =
      Encoding.writeRows(xs)
    def writeWithHeaders(headers: CSV.Headers): CSV.Complete =
      Encoding.writeWithHeaders(xs, headers)
  }

}

object write extends write
