package io.chrisdavenport.cormorant

import io.chrisdavenport.cormorant.Error.ParseFailure
import cats.implicits._
import cats.data._
import atto._
import Atto._

package object parser {

  def parseField(text: String): Either[ParseFailure, CSV.Field] = 
    CSVParser.field.parseOnly(text).either.leftMap(ParseFailure.apply)

  def parseRow(text: String): Either[ParseFailure, CSV.Row] =
    CSVParser.record.parseOnly(text).either.leftMap(ParseFailure.apply)
    

  def parseHeader(text: String): Either[ParseFailure, CSV.Header] =
    CSVParser.name.parseOnly(text).either.leftMap(ParseFailure.apply)

  def parseHeaders(text: String): Either[ParseFailure, CSV.Headers] =
    CSVParser.header.parseOnly(text).either.leftMap(ParseFailure.apply)

  def parseRows(text: String, cleanup: Boolean = true): Either[ParseFailure, CSV.Rows] =
    CSVParser.fileBody
    .parseOnly(text)
    .either
    .leftMap(ParseFailure.apply)
    .map { 
      // Due to The Grammar Being Unclear CRLF can and will be parsed as 
      // a field. However the specification states that each must have the
      // same number of fields. We use this to remove this data we know to
      // be unclear in the specification. In CSV.Rows, we use the first row
      // as the size of reference
      case rows@CSV.Rows(CSV.Row(x) :: _) if cleanup && x.size > 1 => filterLastRowIfEmpty(rows)
      case otherwise => otherwise
    }

  def parseComplete(text: String, cleanup: Boolean = true): Either[ParseFailure, CSV.Complete] =
    CSVParser.`complete-file`
    .parseOnly(text)
    .either
    .leftMap(ParseFailure.apply)
    .map{ case c@CSV.Complete(h@CSV.Headers(headers), rows) => 
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

  def parseTSVField(text: String): Either[ParseFailure, CSV.Field] = 
    TSVParser.field.parseOnly(text).either.leftMap(ParseFailure.apply)

  def parseTSVRow(text: String): Either[ParseFailure, CSV.Row] =
    TSVParser.record.parseOnly(text).either.leftMap(ParseFailure.apply)
    

  def parseTSVHeader(text: String): Either[ParseFailure, CSV.Header] =
    TSVParser.name.parseOnly(text).either.leftMap(ParseFailure.apply)

  def parseTSVHeaders(text: String): Either[ParseFailure, CSV.Headers] =
    TSVParser.header.parseOnly(text).either.leftMap(ParseFailure.apply)

  def parseTSVRows(text: String, cleanup: Boolean = true): Either[ParseFailure, CSV.Rows] =
    TSVParser.fileBody
    .parseOnly(text)
    .either
    .leftMap(ParseFailure.apply)
    .map { 
      // Due to The Grammar Being Unclear CRLF can and will be parsed as 
      // a field. However the specification states that each must have the
      // same number of fields. We use this to remove this data we know to
      // be unclear in the specification. In CSV.Rows, we use the first row
      // as the size of reference
      case rows@CSV.Rows(CSV.Row(x) :: _) if cleanup && x.size > 1 => filterLastRowIfEmpty(rows)
      case otherwise => otherwise
    }

  def parseTSVComplete(text: String, cleanup: Boolean = true): Either[ParseFailure, CSV.Complete] =
    TSVParser.`complete-file`
    .parseOnly(text)
    .either
    .leftMap(ParseFailure.apply)
    .map{ case c@CSV.Complete(h@CSV.Headers(headers), rows) => 
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

  private def filterLastRowIfEmpty(rows: CSV.Rows): CSV.Rows = {
    rows.rows.reverse match {
      case x :: xl if x == CSV.Row(NonEmptyList(CSV.Field(""), Nil)) => CSV.Rows(xl.reverse)
      case _ => rows
    }
  }
}
