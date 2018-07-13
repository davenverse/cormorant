package io.chrisdavenport.cormorant

import io.chrisdavenport.cormorant.Error.ParseFailure
import cats.implicits._
import atto._
import Atto._

package object parser {
  // This is the exact specification
  def parseSpec(text: String): Either[ParseFailure, (Option[CSV.Headers], CSV.Rows)] = 
    CSVParser.file.parseOnly(text).either.leftMap(ParseFailure.apply)

  def parseRows(text: String): Either[ParseFailure, CSV.Rows] = 
    CSVParser.fileBody.parseOnly(text).either.leftMap(ParseFailure.apply)

  def parseComplete(text: String): Either[ParseFailure, CSV.Complete] =
    CSVParser.`complete-file`.parseOnly(text).either.leftMap(ParseFailure.apply)
}