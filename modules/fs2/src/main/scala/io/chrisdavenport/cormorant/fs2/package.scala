package io.chrisdavenport.cormorant

import _root_.fs2._
import cats.implicits._
import atto._
import Atto._

  /**
    * I don't think this is good enough, I think we need a custom pull which emits
    * spec CSV Rows individually
    **/
package object fs2 {
import io.chrisdavenport.cormorant.parser.`package`.CSVParser
import atto.ParseResult

  def parseRowsSafe[F[_]]: Pipe[F, String, Either[Error.ParseFailure, CSV.Row]] =
    _.through(parseN[F, CSV.Row](CSVParser.record <~ opt(CSVParser.PERMISSIVE_CRLF)))
      .map(row => Either.right(row))
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
      _.through(parse1(parser.CSVParser.header <~ parser.CSVParser.PERMISSIVE_CRLF)).map[Either[Error.ParseFailure, (CSV.Headers, Stream[F, String])]]{
        case (ParseResult.Done(rest, a), s) => Either.right((a, Stream(rest) ++ s))
        case (e, _) => e.either.leftMap(Error.ParseFailure.apply).map(h => (h, Stream.empty))
      }.flatMap{_.traverse{
        case (h, s) => s.through(parseN(parser.CSVParser.record <~ opt(parser.CSVParser.PERMISSIVE_CRLF))).map{row => (h, Either.right(row))}
      }}
    }

  private def parse1[F[_], A](p: atto.Parser[A]): Pipe[F, String, (ParseResult[A], Stream[F, String])] = s => {
      def go(r: ParseResult[A])(s: Stream[F, String]): Pull[F, (ParseResult[A], Stream[F, String]), Unit] = {
        r match {
          case p@ParseResult.Partial(_) =>
            s.pull.uncons.flatMap{
              // Add String To Result If Stream Has More Values
              case Some((c, rest)) => go(p.feed(Stream.chunk(c).compile.string))(rest)
              // Reached Stream Termination and Still Partial - Return the partial
              // If we do not call done here, if this can still accept input it will
              // be a partial rather than a done.
              case None => Pull.output1((r.done, Stream.empty))
            }
          case other => Pull.output1((other.done, s))
        }
      }
      go(p.parse(""))(s).stream
    }

  private def parseN[F[_], A](p: Parser[A]): Pipe[F, String, A] = s => {
      def exhaust(r: ParseResult[A], acc: List[A]): (ParseResult[A], List[A]) = {
        r match {
          case ParseResult.Done(in, a) if in === "" =>  (r, a :: acc)
          case ParseResult.Done(in, a) => exhaust(p.parse(in), a :: acc)
          case _           => (r, acc)
        }
      }

  
      def go(r: ParseResult[A])(s: Stream[F, String]): Pull[F, A, Unit] = {
        s.pull.uncons.flatMap{
          case Some((c, rest)) =>
            val s = Stream.chunk(c).compile.string
            val (r0, acc) = r match {
              case ParseResult.Done(in, a)    => (p.parse(in + s), List(a))
              case ParseResult.Fail(_, _, _) => (r, Nil)
              case ParseResult.Partial(_)     => (r.feed(s), Nil)
            }
            val (r1, as) = exhaust(r0, acc)
            Pull.output(Chunk.seq(as.reverse)) >> go(r1)(rest)
          case None => Pull.output(Chunk.seq(exhaust(r.done, Nil)._2))
        }
      }
  
      go(p.parse(""))(s).stream
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

  /**
    * Converts the current `Stream` to a `Stream[F, String]` by encoding its content
    * using the provided `Printer`.
    *
    * This method requires a valid `Write[A]` implicit instance.
    *
    * @example {{{
    * Stream
    *   .emits(list)
    *   .through(writeRows(headers, Printer.default))
    * }}}
    */
  def writeRows[F[_], A: Write](p: Printer): Pipe[F, A, String] = s =>
    s.map(Write[A].write)
      .through(encodeRows(p))
      .intersperse(p.rowSeparator)

  /**
    * Converts the current `Stream` to a `Stream[F, String]` by encoding its content
    * using the provided `Printer` and prepending the provided headers.
    *
    * This method requires a valid `Write[A]` implicit instance.
    *
    * @example {{{
    * Stream
    *   .emits(list)
    *   .through(writeWithHeaders(headers, Printer.default))
    * }}}
    */
  def writeWithHeaders[F[_], A: Write](headers: CSV.Headers, p: Printer): Pipe[F, A, String] = s =>
    Stream(p.print(headers)).covary[F] ++ Stream.emit(p.rowSeparator) ++ s.through(writeRows(p))

  /**
    * Converts the current `Stream` to a `Stream[F, String]` by encoding its content
    * using the provided `Printer` and prepending the headers extracted from a valid
    * `LabelledWrite[A]` implicit instance.
    *
    * @example {{{
    * Stream
    *   .emits(list)
    *   .through(writeLabelled(Printer.default))
    * }}}
    */
  def writeLabelled[F[_], A: LabelledWrite](p: Printer): Pipe[F, A, String] = s =>
    s.through(writeWithHeaders(LabelledWrite[A].headers, p)(new Write[A] {
      override def write(a: A): CSV.Row = LabelledWrite[A].write(a)
    }))

}