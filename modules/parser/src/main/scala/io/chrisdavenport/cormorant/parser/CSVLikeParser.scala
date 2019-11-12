package io.chrisdavenport.cormorant.parser

import io.chrisdavenport.cormorant.CSV
import atto._
import Atto._
import cats.data._
import cats.implicits._
/**
 * This CSVParser tries to stay fairly close to the initial specification
 * https://tools.ietf.org/html/rfc4180
 *
 * Deviations from the specification, here I have chosen to use a
 * permissive CRLF that will accept a CRLF or a LF.
 *
 * The important details are as follows
 * 1.  Each record is located on a separate line, delimited by a line
 *     break (CRLF).  For example:
 *
 *     aaa,bbb,ccc CRLF
 *     zzz,yyy,xxx CRLF
 *
 * 2.  The last record in the file may or may not have an ending line
 *     break.  For example:
 *
 *     aaa,bbb,ccc CRLF
 *     zzz,yyy,xxx
 *
 * 3.  There maybe an optional header line appearing as the first line
 *     of the file with the same format as normal record lines.  This
 *     header will contain names corresponding to the fields in the file
 *     and should contain the same number of fields as the records in
 *     the rest of the file (the presence or absence of the header line
 *     should be indicated via the optional "header" parameter of this
 *     MIME type).  For example:
 *
 *     field_name,field_name,field_name CRLF
 *     aaa,bbb,ccc CRLF
 *     zzz,yyy,xxx CRLF
 * 4.  Within the header and each record, there may be one or more
 *     fields, separated by commas.  Each line should contain the same
 *     number of fields throughout the file.  Spaces are considered part
 *     of a field and should not be ignored.  The last field in the
 *     record must not be followed by a comma.  For example:
 *
 *     aaa,bbb,ccc
 *
 * 5.  Each field may or may not be enclosed in double quotes (however
 *     some programs, such as Microsoft Excel, do not use double quotes
 *     at all).  If fields are not enclosed with double quotes, then
 *     double quotes may not appear inside the fields.  For example:
 *
 *     "aaa","bbb","ccc" CRLF
 *     zzz,yyy,xxx
 *
 * 6.  Fields containing line breaks (CRLF), double quotes, and commas
 *     should be enclosed in double-quotes.  For example:
 *
 *     "aaa","b CRLF
 *     bb","ccc" CRLF
 *     zzz,yyy,xxx
 *
 * 7.  If double-quotes are used to enclose fields, then a double-quote
 *     appearing inside a field must be escaped by preceding it with
 *     another double quote.  For example:
 *
 *     "aaa","b""bb","ccc"
 **/
abstract class CSVLikeParser(val separator: Char) {
  val dquote: Char = '\"'
  val dquoteS: String = dquote.toString
  val separatorS: String = separator.toString
  val cr: Char = '\r'
  val crS: String = cr.toString
  val lf: Char = '\n'
  val lfS: String = lf.toString

  // DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]
  val DQUOTE: Parser[Char] = char(dquote)
  // Used For Easier Composition in escaped spec is referred to as 2DQUOTE
  val TWO_DQUOTE: Parser[(Char, Char)] = DQUOTE ~ DQUOTE
  //CR = %x0D ;as per section 6.1 of RFC 2234 [2]
  val CR: Parser[Char] = char(cr)
  // LF = %x0A ;as per section 6.1 of RFC 2234 [2]
  val LF: Parser[Char] = char(lf)
  // CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]
  val CRLF: Parser[(Char, Char)] = CR ~ LF

  // Genuine CRLF or a Line Feed which is translated to a CRLF
  val PERMISSIVE_CRLF: Parser[(Char, Char)] =
    (CRLF | char(lf).map((cr, _))).named("PERMISSIVE_CRLF")

  // COMMA = %x2C
  val SEPARATOR: Parser[Char] = char(separator).named("SEPARATOR")

  // TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
  val TEXTDATA: Parser[Char] = noneOf(dquoteS + separatorS + crS + lfS).named("TEXTDATA")

  // escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
  val escaped: Parser[CSV.Field] =
    (DQUOTE ~> many(TEXTDATA | SEPARATOR | CR | LF | TWO_DQUOTE.map(_ => dquote))
      .map(_.mkString)
      .map(CSV.Field) <~ DQUOTE)
      .named("escaped")

  val `non-escaped`: Parser[CSV.Field] = many(TEXTDATA)
    .map(_.mkString)
    .map(CSV.Field)
    .named("non-escaped")

  // field = (escaped / non-escaped)
  val field: Parser[CSV.Field] = (escaped | `non-escaped`)
    .named("CSV.Field")

  // name = field
  val name: Parser[CSV.Header] = field
    .map(f => CSV.Header(f.x))
    .named("CSV.Header")
  // header = name *(COMMA name)
  val header: Parser[CSV.Headers] = (name, many(SEPARATOR ~> name))
    .mapN(NonEmptyList(_, _))
    .map(CSV.Headers)
    .named("CSV.Headers")

  // record = field *(COMMA field)
  val record: Parser[CSV.Row] = (field, many(SEPARATOR ~> field))
    .mapN(NonEmptyList(_, _))
    .map(CSV.Row)
    .named("CSV.Row")

  val fileBody: Parser[CSV.Rows] =
    (record, many(PERMISSIVE_CRLF ~> record), opt(PERMISSIVE_CRLF))
      .mapN((h, t, _) => h :: t)
      .map(CSV.Rows)
      .named("CSV.Rows")

  // file = [header CRLF] record *(CRLF record) [CRLF]
  // val file: Parser[(Option[CSV.Headers], CSV.Rows)] =
  //   (opt(header <~ PERMISSIVE_CRLF), fileBody)
  //     .mapN((_, _))
  //     .named("CSV Specification")

  val `complete-file`: Parser[CSV.Complete] =
    ((header <~ PERMISSIVE_CRLF), fileBody)
      .mapN(CSV.Complete)
      .named("CSV.Complete")

}
