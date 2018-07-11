package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant._

trait read {
  implicit class readRow(csv: CSV.Row){
    def read[A: Read]: Either[Error.DecodeFailure, A] = Read[A].read(csv)
  }
  implicit class readRows(csv: CSV.Rows){
    def readAll[A: Read]: List[Either[Error.DecodeFailure, A]] = csv.rows.map(Read[A].read)
  }

  implicit class readComplete(csv: CSV.Complete){
    def readAll[A: Read]: List[Either[Error.DecodeFailure, A]] = csv.rows.rows.map(Read[A].read)
  }
}

object read extends read