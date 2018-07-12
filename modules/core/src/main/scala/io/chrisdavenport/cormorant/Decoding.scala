package io.chrisdavenport.cormorant

object Decoding {

  def readRow[A: Read](row: CSV.Row): Either[Error.DecodeFailure, A] = Read[A].read(row)
  def readRows[A: Read](rows: CSV.Rows): List[Either[Error.DecodeFailure, A]] = rows.rows.map(Read[A].read)
  def readComplete[A: Read](complete: CSV.Complete): List[Either[Error.DecodeFailure, A]] = readRows(complete.rows)

  def readLabelled[A: LabelledRead](complete: CSV.Complete): List[Either[Error.DecodeFailure, A]] = 
    complete.rows.rows.map(LabelledRead[A].read(_, complete.headers))


}