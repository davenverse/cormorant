package io.chrisdavenport.cormorant

import _root_.fs2._
import cats.implicits._

  /**
    * I don't think this is good enough, I think we need a custom pull which emits
    * spec CSV Rows individually
    **/
package object fs2 {

  def parseRowsSafe[F[_]]: Pipe[F, String, Either[Error.ParseFailure, CSV.Row]] =
    _.map(parser.parseRow)
  def parseRows[F[_]: RaiseThrowable]: Pipe[F, String, CSV.Row] =
    _.through(parseRowsSafe).rethrow

  def readRowsSafe[F[_], A: Read]: Pipe[F, String, Either[Error, A]] = 
    _.through(parseRowsSafe).map(_.leftWiden.flatMap(Read[A].read))
  def readRows[F[_]: RaiseThrowable, A: Read]: Pipe[F, String, A] = _.through(readRowsSafe).rethrow

  /**
    * Read the first line as the headers, the rest as rows.
    * This is super general to allow for better combinators based on it
    **/
  def parseCompleteSafe[F[_]]: Pipe[F, String, 
    Either[Error.ParseFailure,(CSV.Headers, Either[Error.ParseFailure, CSV.Row])]] = {
      def partialParseComplete: Pipe[F, String, Either[Error.ParseFailure, (CSV.Headers, Stream[F,String])]] = 
        _.pull.uncons1.flatMap{
        case Some((headers, s)) => 
          val headersParsed : Either[Error.ParseFailure, CSV.Headers] = parser.parseHeaders(headers)
          Pull.output1(headersParsed.map((_, s))) >> Pull.done
        case None =>
          Pull.done
      }.stream

      _.through(partialParseComplete).flatMap(_.traverse{
        case (h, s) => s.map(s => (h, parser.parseRow(s)))
      })
    }

  def parseComplete[F[_]: RaiseThrowable]: Pipe[F, String, (CSV.Headers, CSV.Row)] =
    _.through(parseCompleteSafe).rethrow.map{case (h, e) => e.map((h, _))}.rethrow
  
  def readLabelledCompleteSafe[F[_], A: LabelledRead]: Pipe[F, String, Either[Error, A]] =
    _.through(parseCompleteSafe).map{e => 
      for {
        (h, eRow) <- e
        row <- eRow
        a <- LabelledRead[A].read(row, h)
      } yield a
    }

  def readLabelled[F[_]: RaiseThrowable, A: LabelledRead]: Pipe[F, String, A] =
    _.through(readLabelledCompleteSafe).rethrow


  def encodeRows[F[_]](p: Printer): Pipe[F, CSV.Row, String] = 
    _.map(p.print)

  def writeRows[F[_], A: Write](p: Printer): Pipe[F, A, String] = 
    _.map(Write[A].write).through(encodeRows(p))

  def writeWithHeaders[F[_], A: Write](headers: CSV.Headers, p: Printer): Pipe[F, A, String] = s =>
    Stream(p.print(headers)).covary[F] ++ s.through(writeRows(p))

  def writeLabelled[F[_], A: LabelledWrite](p: Printer): Pipe[F, A, String] = s =>
    s.through(writeWithHeaders(LabelledWrite[A].headers, p)(LabelledWrite[A].write))

}