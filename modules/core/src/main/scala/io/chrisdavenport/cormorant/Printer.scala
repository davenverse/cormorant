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


  /**
   * Leading characters that a spreadsheet application may treat as the start of
   * a formula when a CSV file is opened directly, rather than imported.
   */
  val formulaLeadingCharacters: Set[Char] = Set('=', '+', '-', '@', '\t', '\r')

  /**
   * Prefixes a value with a single quote when it begins with a character a
   * spreadsheet would read as a formula. The quote is not part of the data and
   * is stripped by spreadsheet applications on display.
   */
  private[cormorant] def escapedFormula(string: String): String =
    if (string.headOption.exists(formulaLeadingCharacters.contains)) "'" + string
    else string

  private def genericImpl(
      columnSeperator: String,
      rowSeperator: String,
      escape: String,
      surround: String,
      additionalEscapes: Set[String],
      escapeFormulas: Boolean): Printer =
    new Printer {

      private def field(text: String): String = {
        // Formula escaping happens first so the added quote sits inside any
        // RFC 4180 surrounding rather than outside it.
        val formulaSafe = if (escapeFormulas) escapedFormula(text) else text
        escapedAsNecessary(
          formulaSafe,
          Set(columnSeperator, rowSeperator, escape, surround) ++ additionalEscapes,
          escape,
          surround)
      }

      override def print(csv: CSV): String = csv match {
        case CSV.Field(text) => field(text)
        case CSV.Header(text) => field(text)
        case CSV.Row(xs) => xs.map(print).intercalate(columnSeperator)
        case CSV.Headers(xs) => xs.map(print).intercalate(columnSeperator)
        case CSV.Rows(xs) => xs.map(print).intercalate(rowSeperator)
        case CSV.Complete(headers, body) => print(headers) + rowSeperator + print(body)
      }

      override val rowSeparator: String = rowSeperator

    }

  def generic(
      columnSeperator: String,
      rowSeperator: String,
      escape: String,
      surround: String,
      additionalEscapes: Set[String] = Set.empty[String]): Printer =
    genericImpl(columnSeperator, rowSeperator, escape, surround, additionalEscapes, false)

  /**
   * As [[generic]], but additionally guards against CSV formula injection.
   *
   * Output is only safe to open directly in a spreadsheet if the values cannot
   * be interpreted as formulas. This variant is opt-in because prefixing
   * changes the bytes written, which matters when the output is consumed by
   * another program rather than a human.
   */
  def genericEscapingFormulas(
      columnSeperator: String,
      rowSeperator: String,
      escape: String,
      surround: String,
      additionalEscapes: Set[String] = Set.empty[String]): Printer =
    genericImpl(columnSeperator, rowSeperator, escape, surround, additionalEscapes, true)

  def default: Printer = generic(",", "\n", "\"", "\"", Set("\r"))
  def tsv: Printer = generic("\t", "\n", "\"", "\"", Set("\r"))

  /** [[default]] with formula-injection escaping. See [[genericEscapingFormulas]]. */
  def defaultEscapingFormulas: Printer =
    genericEscapingFormulas(",", "\n", "\"", "\"", Set("\r"))

  /** [[tsv]] with formula-injection escaping. See [[genericEscapingFormulas]]. */
  def tsvEscapingFormulas: Printer =
    genericEscapingFormulas("\t", "\n", "\"", "\"", Set("\r"))

}
