package io.chrisdavenport.cormorant

trait LabelledRead[A] {
  def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure, A]
}
object LabelledRead {
  def apply[A](implicit ev: LabelledRead[A]): LabelledRead[A] = ev

  /**
   * Labelled Read Which Ignores Headers and Reads Based on the Supplied Read
    **/
  def fromRead[A: Read]: LabelledRead[A] = new LabelledRead[A] {
    def read(a: CSV.Row, h: CSV.Headers): Either[Error.DecodeFailure, A] =
      Read[A].read(a)
  }
}
