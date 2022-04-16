package io.chrisdavenport.cormorant

import cats.syntax.all._

trait Printer {
  def print(csv: CSV): String
  val rowSeparator: String
}

object Printer {
  //
  private[cormorant] def escapedAsNecessary(
      string: String,
      stringsToEscape: Set[String],
      escape: String,
      surround: String
  ): String = {

    if (stringsToEscape.exists(string.contains(_))) {
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
      surround: String,
      additionalEscapes: Set[String] = Set.empty[String]): Printer =
    new Printer {

      override def print(csv: CSV): String = csv match {
        case CSV.Field(text) =>
          escapedAsNecessary(text, Set(columnSeperator, rowSeperator, escape, surround) ++ additionalEscapes, escape, surround)
        case CSV.Header(text) =>
          escapedAsNecessary(text, Set(columnSeperator, rowSeperator, escape, surround) ++ additionalEscapes, escape, surround)
        case CSV.Row(xs) => xs.map(print).intercalate(columnSeperator)
        case CSV.Headers(xs) => xs.map(print).intercalate(columnSeperator)
        case CSV.Rows(xs) => xs.map(print).intercalate(rowSeperator)
        case CSV.Complete(headers, body) => print(headers) + rowSeperator + print(body)
      }

      override val rowSeparator: String = rowSeperator

    }

  def default: Printer = generic(",", "\n", "\"", "\"", Set("\r"))
  def tsv: Printer = generic("\t", "\n", "\"", "\"", Set("\r"))

}
