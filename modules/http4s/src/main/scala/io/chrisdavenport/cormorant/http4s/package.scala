package io.chrisdavenport.cormorant

import cats._
import cats.effect.Sync
import cats.implicits._
import fs2._
import org.http4s._

package object http4s {

  implicit def completeEntityEncoder[F[_]: Applicative](
    printer: Printer = Printer.default, 
    mediaType: MediaType = MediaType.`text/csv`
  ): EntityEncoder[F, CSV.Complete] = {
    val contentTypeHeader: Header = headers.`Content-Type`(mediaType)
    EntityEncoder.encodeBy(Headers(contentTypeHeader))(csvComplete =>
      Entity(Stream(printer.print(csvComplete)).through(fs2.text.utf8Encode).covary[F]).pure[F]
    )
  }
  implicit def rowsEntityEncoder[F[_]: Applicative](
    printer: Printer = Printer.default, 
    mediaType: MediaType = MediaType.`text/csv`
  ): EntityEncoder[F, CSV.Complete] = {
    val contentTypeHeader: Header = headers.`Content-Type`(mediaType)
    EntityEncoder.encodeBy(Headers(contentTypeHeader))(csvComplete =>
      Entity(Stream(printer.print(csvComplete)).through(fs2.text.utf8Encode).covary[F]).pure[F]
    )
  }

  implicit def completeEntityDecoder[F[_]: Sync]: EntityDecoder[F, CSV.Complete] = 
    new EntityDecoder[F, CSV.Complete]{
      def consumes: Set[MediaRange] = Set(MediaType.`text/csv`)
      def decode(msg: Message[F],strict: Boolean): DecodeResult[F,CSV.Complete] = cats.data.EitherT{
        msg.body.through(fs2.text.utf8Decode).compile.foldMonoid.map(s =>
          parser.parseComplete(s).leftMap(parseError => org.http4s.MalformedMessageBodyFailure(parseError.reason))
        )
      }
    }

  implicit def rowsEntityDecoder[F[_]: Sync]: EntityDecoder[F, CSV.Rows] = 
    new EntityDecoder[F, CSV.Rows]{
      def consumes: Set[MediaRange] = Set(MediaType.`text/csv`)
      def decode(msg: Message[F],strict: Boolean): DecodeResult[F,CSV.Rows] = cats.data.EitherT{
        msg.body.through(fs2.text.utf8Decode).compile.foldMonoid.map(s =>
          parser.parseRows(s).leftMap(parseError => org.http4s.MalformedMessageBodyFailure(parseError.reason))
        )
      }
    }

}