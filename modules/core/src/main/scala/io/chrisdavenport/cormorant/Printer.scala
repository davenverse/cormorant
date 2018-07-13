package io.chrisdavenport.cormorant

import cats.implicits._

trait Printer {
  def print(csv: CSV): String
}

object Printer {
  //
  private[cormorant] def escapedAsNecessary(
      string: String,
      columnSeperator: String,
      rowSeperator: String,
      escape: String,
      surround: String
  ): String = {

    if (string.contains(columnSeperator) || string.contains(rowSeperator)) {
      val escapedString = string.replace(surround, escape + surround)
      surround + escapedString + surround
    } else {
      string
    }

  }

  def generic(
      columnSeperator: String,
      rowSeperator: String,
      escape: String,
      surround: String): Printer =
    new Printer {
      def print(csv: CSV): String = csv match {
        case CSV.Field(text) =>
          escapedAsNecessary(text, columnSeperator, rowSeperator, escape, surround)
        case CSV.Header(text) =>
          escapedAsNecessary(text, columnSeperator, rowSeperator, escape, surround)
        case CSV.Row(xs) => xs.map(print).intercalate(columnSeperator)
        case CSV.Headers(xs) => xs.map(print).intercalate(columnSeperator)
        case CSV.Rows(xs) => xs.map(print).intercalate(rowSeperator)
        case CSV.Complete(headers, body) => print(headers) + rowSeperator + print(body)
      }
    }

  def default: Printer = generic(",", "\n", "\"", "\"")
  def tsv: Printer = generic("\t", "\n", "\"", "\"")

}
