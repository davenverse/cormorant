package io.chrisdavenport.cormorant
package catsparse

import io.chrisdavenport.cormorant.Error.ParseFailure
import cats.implicits._
import cats.data._
import cats.parse.Parser0

package object parser {

  object CSVParser extends CSVLikeParser(',')

  def parseOnly[A](p: Parser0[A], s: String): Either[ParseFailure, A] =
    p.parse(s).leftMap(err => ParseFailure(err.toString())).map(_._2)

  def parseField(text: String, parser: CSVLikeParser = CSVParser): Either[ParseFailure, CSV.Field] =
    parseOnly(parser.field, text)

  def parseRow(text: String, parser: CSVLikeParser = CSVParser): Either[ParseFailure, CSV.Row] =
    parseOnly(parser.record, text)

  def parseHeader(
      text: String,
      parser: CSVLikeParser = CSVParser
  ): Either[ParseFailure, CSV.Header] =
    parseOnly(parser.name, text)

  def parseHeaders(
      text: String,
      parser: CSVLikeParser = CSVParser
  ): Either[ParseFailure, CSV.Headers] =
    parseOnly(parser.header, text)

  def parseRows(
      text: String,
      cleanup: Boolean = true,
      parser: CSVLikeParser = CSVParser
  ): Either[ParseFailure, CSV.Rows] =
    parseOnly(parser.fileBody, text)
      .map {
        // Due to The Grammar Being Unclear CRLF can and will be parsed as
        // a field. However the specification states that each must have the
        // same number of fields. We use this to remove this data we know to
        // be unclear in the specification. In CSV.Rows, we use the first row
        // as the size of reference
        case rows @ CSV.Rows(CSV.Row(x) :: _) if cleanup && x.size > 1 => filterLastRowIfEmpty(rows)
        case otherwise => otherwise
      }

  def parseComplete(
      text: String,
      cleanup: Boolean = true,
      parser: CSVLikeParser = CSVParser
  ): Either[ParseFailure, CSV.Complete] =
    parseOnly(parser.`complete-file`, text)
      .map {
        case c @ CSV.Complete(h @ CSV.Headers(headers), rows) =>
          // Due to The Grammar Being Unclear CRLF can and will be parsed as
          // a field. However the specification states that each must have the
          // same number of fields. We use this to remove this data we know to
          // be unclear in the specification. In CSV.Complete, we use headers
          // as the size of reference.
          if (cleanup && headers.size > 1) {
            CSV.Complete(h, filterLastRowIfEmpty(rows))
          } else {
            c
          }
      }

  object TSVParser extends CSVLikeParser('\t')
  def parseTSVField(text: String): Either[ParseFailure, CSV.Field] = parseField(text, TSVParser)

  def parseTSVRow(text: String): Either[ParseFailure, CSV.Row] = parseRow(text, TSVParser)

  def parseTSVHeader(text: String): Either[ParseFailure, CSV.Header] = parseHeader(text, TSVParser)

  def parseTSVHeaders(text: String): Either[ParseFailure, CSV.Headers] =
    parseHeaders(text, TSVParser)

  def parseTSVRows(text: String, cleanup: Boolean = true): Either[ParseFailure, CSV.Rows] =
    parseRows(text, cleanup, TSVParser)

  def parseTSVComplete(text: String, cleanup: Boolean = true): Either[ParseFailure, CSV.Complete] =
    parseComplete(text, cleanup, TSVParser)

  private def filterLastRowIfEmpty(rows: CSV.Rows): CSV.Rows = {
    rows.rows.reverse match {
      case x :: xl if x == CSV.Row(NonEmptyList(CSV.Field(""), Nil)) => CSV.Rows(xl.reverse)
      case _ => rows
    }
  }
}

