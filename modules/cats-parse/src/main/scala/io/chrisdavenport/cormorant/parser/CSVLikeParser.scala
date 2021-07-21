package io.chrisdavenport.cormorant.catsparse

import io.chrisdavenport.cormorant.CSV
import cats.data._
import cats.implicits._
import cats.parse.{Parser, Parser0}
import cats.parse.Parser.{char, charWhere}
/**
 * This CSVParser tries to stay fairly close to the initial specification
 * https://tools.ietf.org/html/rfc4180
 *
 * Deviations from the specification, here I have chosen to use a
 * permissive CRLF that will accept a CRLF, LF, or CR.
 * Note that the CR is not directly in the initial spec, but in rare
 * cases csvs can have this delimiter
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
  val DQUOTE = char(dquote)
  // Used For Easier Composition in escaped spec is referred to as 2DQUOTE
  val TWO_DQUOTE = (DQUOTE ~ DQUOTE).backtrack
  //CR = %x0D ;as per section 6.1 of RFC 2234 [2]
  val CR = char(cr)
  // LF = %x0A ;as per section 6.1 of RFC 2234 [2]
  val LF = char(lf)
  // CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]
  val CRLF = CR ~ LF

  // Genuine CRLF, a Line Feed, or a CR which is translated to a CRLF
  val PERMISSIVE_CRLF =
    (CRLF | LF.map((cr, _)) | CR.map((_, lf)))

  // COMMA = %x2C
  val SEPARATOR = char(separator)

  // TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
  val badChars = List(dquote, separator, cr, lf)
  val TEXTDATA: Parser[Char] = charWhere(c => !badChars.contains(c))

  // escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
  val escapedChar: Parser[String] = (TEXTDATA | SEPARATOR | CR | LF).string | TWO_DQUOTE.string.as(dquoteS)
  val escaped: Parser0[CSV.Field] =
    escapedChar
      .rep0
      .map(ss => CSV.Field(ss.mkString))
      .surroundedBy(DQUOTE)

  val `non-escaped`: Parser0[CSV.Field] = TEXTDATA
    .rep0.string
    .map(CSV.Field)

  // field = (escaped / non-escaped)
  val field: Parser0[CSV.Field] = (escaped | `non-escaped`)

  // name = field
  val name: Parser0[CSV.Header] = field
    .map(f => CSV.Header(f.x))
  // header = name *(COMMA name)
  val header: Parser0[CSV.Headers] = (name, (SEPARATOR *> name).rep0)
    .mapN(NonEmptyList(_, _))
    .map(CSV.Headers)

  // record = field *(COMMA field)
  val record: Parser0[CSV.Row] = (field, (SEPARATOR *> field).rep0)
    .mapN(NonEmptyList(_, _))
    .map(CSV.Row)

  val fileBody: Parser0[CSV.Rows] =
    (record, (PERMISSIVE_CRLF *> record).rep0, PERMISSIVE_CRLF.?)
      .mapN((h, t, _) => h :: t)
      .map(CSV.Rows)

  // file = [header CRLF] record *(CRLF record) [CRLF]
  // val file: Parser[(Option[CSV.Headers], CSV.Rows)] =
  //   (opt(header <~ PERMISSIVE_CRLF), fileBody)
  //     .mapN((_, _))
  //     .named("CSV Specification")

  val `complete-file`: Parser0[CSV.Complete] =
    ((header <* PERMISSIVE_CRLF), fileBody)
      .mapN(CSV.Complete)

}
