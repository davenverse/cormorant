package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant._

trait write {
  implicit class writeOps[A: Write](a: A){
    def write: CSV.Row = Write[A].write(a)
  }

  implicit class encodingOps[A: Write](xs: List[A]){
    def encodeRows: CSV.Rows = 
      Encoding.encodeRows(xs)
    def encodeWithHeaders(headers: CSV.Headers): CSV.Complete =
      Encoding.encodeWithHeaders(xs, headers)
  }
}