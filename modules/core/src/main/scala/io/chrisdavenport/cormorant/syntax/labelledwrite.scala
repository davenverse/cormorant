package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant._

trait labelledwrite {
  implicit class labelledWriteOps[A: LabelledWrite](a: A) {
    def write: CSV.Row = LabelledWrite[A].write(a)
  }

  implicit class labelledWriteListOps[A: LabelledWrite](xs: List[A]) {
    def writeComplete: CSV.Complete = Encoding.writeComplete(xs)
  }

}

object labelledwrite extends labelledwrite
