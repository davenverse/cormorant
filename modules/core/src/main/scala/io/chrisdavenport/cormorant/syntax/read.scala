package io.chrisdavenport.cormorant.syntax

import io.chrisdavenport.cormorant._

trait read {
  implicit class readRow(csv: CSV.Row){
    def readRow[A: Read]: Either[Error.DecodeFailure, A] = Decoding.readRow(csv)
  }
  implicit class readRows(csv: CSV.Rows){
    def readRows[A: Read]: List[Either[Error.DecodeFailure, A]] = Decoding.readRows(csv)
  }

  implicit class readComplete(csv: CSV.Complete){
    def readComplete[A: Read]: List[Either[Error.DecodeFailure, A]] = csv.rows.rows.map(Read[A].read)
  }
}

object read extends read