package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant._

trait labelledwrite {
  implicit class labelledWriteOps[A: LabelledWrite](a: A){
    def write: CSV.Row = LabelledWrite[A].write(a)
  }

  implicit class labelledWriteListOps[A: LabelledWrite](xs: List[A]){
    def encode: CSV.Complete = Encoding.encode(xs)
  }

}

object labelledwrite extends labelledwrite