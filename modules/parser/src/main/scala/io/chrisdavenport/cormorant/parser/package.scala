package io.chrisdavenport.cormorant

import io.chrisdavenport.cormorant.Error.ParseFailure
import cats.implicits._
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

  def parseRows(text: String): Either[ParseFailure, CSV.Rows] =
    CSVParser.fileBody.parseOnly(text).either.leftMap(ParseFailure.apply)

  def parseComplete(text: String): Either[ParseFailure, CSV.Complete] =
    CSVParser.`complete-file`.parseOnly(text).either.leftMap(ParseFailure.apply)
}
