package io.chrisdavenport.cormorant.poi

import cats.syntax.all._
import org.apache.poi.xssf.usermodel.XSSFSheet
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import io.chrisdavenport.cormorant.CSV
import org.apache.poi.ss.usermodel.CellType._
import cats.effect._
import fs2._

object POI {

  private object JDKCollectionConvertersCompat {
    object Scope1 {
      object jdk {
        type CollectionConverters = Int
      }
    }
    import Scope1._

    object Scope2 {
      import scala.collection.{JavaConverters => CollectionConverters}
      object Inner {
        import scala._
        import jdk.CollectionConverters
        val Converters = CollectionConverters
      }
    }

    val Converters = Scope2.Inner.Converters
  }

  import JDKCollectionConvertersCompat.Converters._

  private def fromSheetInternal(sheet: XSSFSheet): Either[Throwable, List[List[String]]] = Either.catchNonFatal{
    val lb : scala.collection.mutable.ListBuffer[List[String]] = 
      scala.collection.mutable.ListBuffer.empty[List[String]]
    
    sheet.iterator().asScala.foreach{row =>
      val lr: scala.collection.mutable.ListBuffer[String] = 
        scala.collection.mutable.ListBuffer.empty[String]

      row.cellIterator.asScala.foreach{ cell =>
        val string = cell.getCellType match {
          case BOOLEAN => cell.getBooleanCellValue().toString()
          case STRING => cell.getStringCellValue()
          case NUMERIC => cell.getNumericCellValue().toString()
          case BLANK => "" // TODO: Maybe an Error, Not sure
          case ERROR => throw new Throwable(s"No Errors Allowed - $cell")
          case FORMULA => 
            // Evaluate Formulas to Values
            cell.getCachedFormulaResultType() match {
              case BOOLEAN => cell.getBooleanCellValue().toString()
              case STRING => cell.getStringCellValue()
              case NUMERIC => cell.getNumericCellValue().toString()
              case BLANK => "" // TODO: Maybe an Error, Not sure
              case ERROR => throw new Throwable(s"No Errors Allowed - $cell")
              case FORMULA => throw new Throwable(s"No Formulas Allowed as Formula Results - $cell")
              case _NONE@(_) => throw new Throwable(s"No _None allowed - Not sure what this is $cell")
            }
          case _NONE@(_) => throw new Throwable(s"No _None allowed - Not sure what this is $cell")
        }
        lr += string
      }
      val out = lr.toList
      lb += out
    }

    lb.toList
  }

  private def fromLLComplete(l: List[List[String]]): Either[Throwable, CSV.Complete] = 
    l.toNel.toRight(new Throwable("No Rows Found")).flatMap(l => 
      l.zipWithIndex.traverse{ case (l, int) => l.toNel.toRight(new Throwable(s"No Columns Found For Row $int"))}
    ).map{
      case cats.data.NonEmptyList(h, body) => 
        val headers = CSV.Headers(h.map(CSV.Header))
        val rows = CSV.Rows(body.map(row => CSV.Row(row.map(CSV.Field(_)))))
        CSV.Complete(headers, rows)
    }

  private def fromLLRows(l: List[List[String]]): Either[Throwable, CSV.Rows] = {
    l.zipWithIndex.traverse{ case(l, int) => l.toNel.toRight(new Throwable(s"No Columns Found For Row $int"))}
    .map{ body => 
      CSV.Rows(body.map(row => CSV.Row(row.map(CSV.Field(_)))))
    }
  }

  def fromSheetComplete(sheet: XSSFSheet): Either[Throwable, CSV.Complete] =
    fromSheetInternal(sheet).flatMap(fromLLComplete)

  def fromSheetRows(sheet: XSSFSheet): Either[Throwable, CSV.Rows] =
    fromSheetInternal(sheet).flatMap(fromLLRows)

  def fromWorkbookComplete(wb: XSSFWorkbook, sheet: Either[Int, String] = Left(1)): Either[Throwable, CSV.Complete] = 
    Either.catchNonFatal(sheet.fold(wb.getSheetAt(_), wb.getSheet(_)))
      .flatMap(fromSheetComplete)

  def fromWorkbookRows(wb: XSSFWorkbook, sheet: Either[Int, String] = Left(1)): Either[Throwable, CSV.Rows] =
    Either.catchNonFatal(sheet.fold(wb.getSheetAt(_), wb.getSheet(_)))
      .flatMap(fromSheetRows)

  def fromStreamComplete[F[_]: ConcurrentEffect](s: Stream[F, Byte], sheet: Either[Int, String] = Left(1)): F[CSV.Complete] = 
    s.through(fs2.io.toInputStream)
      .map(is => new XSSFWorkbook(is))
      .evalMap(wb => fromWorkbookComplete(wb, sheet).liftTo[F])
      .compile
      .lastOrError

  def fromStreamRows[F[_]: ConcurrentEffect](s: Stream[F, Byte], sheet: Either[Int, String] = Left(1)): F[CSV.Rows] = 
    s.through(fs2.io.toInputStream)
      .map(is => new XSSFWorkbook(is))
      .evalMap(wb => fromWorkbookRows(wb, sheet).liftTo[F])
      .compile
      .lastOrError
}
