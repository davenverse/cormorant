package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant._

trait labelledread {

  implicit class labelledreadCompleteOps(csv: CSV.Complete){
    def readLabelled[A: LabelledRead]: List[Either[Error.DecodeFailure, A]] = Decoding.readLabelled(csv)
  }

}

object labelledread extends labelledread