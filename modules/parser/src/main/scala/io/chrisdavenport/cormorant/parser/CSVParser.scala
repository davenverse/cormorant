package io.chrisdavenport.cormorant.parser

import io.chrisdavenport.cormorant.CSV

import atto._
import Atto._
import cats.implicits._

object CSVParser {

  val dquote : Char = '\"'
  val dquoteS: String = dquote.toString
  val comma : Char = ','
  val commaS : String = comma.toString
  val cr : Char = '\r'
  val crS: String = cr.toString
  val lf : Char = '\n'
  val lfS : String = lf.toString
  
  // DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]
  val DQUOTE : Parser[Char] = char(dquote)
  //CR = %x0D ;as per section 6.1 of RFC 2234 [2]
  val TWO_DQUOTE : Parser[(Char, Char)]  = DQUOTE ~ DQUOTE
  val CR : Parser[Char] = char(cr)
  // LF = %x0A ;as per section 6.1 of RFC 2234 [2]
  val LF : Parser[Char] = char(lf)
  // CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]
  val CRLF : Parser[(Char, Char)] = CR ~ LF

  // Genuine CRLF or a Line Feed which is translated to a CRLF
  val PERMISSIVE_CRLF : Parser[(Char, Char)] = CRLF | char(lf).map((cr, _))

  // COMMA = %x2C
  val COMMA : Parser[Char] = char(comma)

  // TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
  val TEXTDATA : Parser[Char] = noneOf(dquoteS + commaS + crS + lfS)

  // escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
  val escaped : Parser[CSV.Field] = 
    DQUOTE ~> many(TEXTDATA | COMMA | CR | LF | TWO_DQUOTE.map(_ => dquote)).map(_.mkString).map(CSV.Field) <~ DQUOTE

  val `non-escaped` : Parser[CSV.Field] = many(TEXTDATA).map(_.mkString).map(CSV.Field)

  // field = (escaped / non-escaped)
  val field : Parser[CSV.Field] = escaped | `non-escaped`

  // name = field
  val name : Parser[CSV.Header] = field.map(f => CSV.Header(f.x))
  // val name : Parser[CSV.Header] = takeWhile(_ != ',').map(CSV.Header)
  //   .named("CSV.Header")
  // header = name *(COMMA name)
  val header : Parser[CSV.Headers] = (name, many(COMMA ~> name)).mapN(_ :: _).map(CSV.Headers).named("CSV.Headers")

  // record = field *(COMMA field)
  val record : Parser[CSV.Row] = (field, COMMA ~> many(field)).mapN(_ :: _).map(CSV.Row).named("CSV.Row")

  val fileBody: Parser[CSV.Rows] =
    (record, many(PERMISSIVE_CRLF ~> record), opt(PERMISSIVE_CRLF)).mapN((h, t, _) => h :: t).map(CSV.Rows)
      .named("CSV.Rows")

  // file = [header CRLF] record *(CRLF record) [CRLF]
  val file : Parser[(Option[CSV.Headers], CSV.Rows)] =
    (opt(header <~ PERMISSIVE_CRLF), fileBody).mapN((_, _))
      .named("CSV Specification")

  val `complete-file` : Parser[CSV.Complete] =
    ((header <~ PERMISSIVE_CRLF), fileBody).mapN(CSV.Complete)
      .named("CSV.Complete")


}