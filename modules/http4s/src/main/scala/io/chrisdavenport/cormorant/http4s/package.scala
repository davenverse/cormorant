package io.chrisdavenport.cormorant

import cats.effect.Sync
import cats.implicits._
import _root_.fs2._
import _root_.io.chrisdavenport.cormorant.{fs2 => _}
import _root_.io.chrisdavenport.cormorant.fs2._
import org.http4s._

package object http4s {

  implicit def completeEntityEncoder[F[_]](
      implicit
      printer: Printer = Printer.default,
      mediaType: MediaType = MediaType.text.csv
  ): EntityEncoder[F, CSV.Complete] = {
    val contentTypeHeader = headers.`Content-Type`(mediaType)
    EntityEncoder.encodeBy(contentTypeHeader)(
      csvComplete => Entity(Stream(printer.print(csvComplete)).through(text.utf8Encode).covary[F])
    )
  }
  implicit def rowsEntityEncoder[F[_]](
      implicit
      printer: Printer = Printer.default,
      mediaType: MediaType = MediaType.text.csv
  ): EntityEncoder[F, CSV.Rows] = {
    val contentTypeHeader = headers.`Content-Type`(mediaType)
    EntityEncoder.encodeBy(contentTypeHeader)(
      csvRows => Entity(Stream(printer.print(csvRows)).through(text.utf8Encode).covary[F])
    )
  }

  implicit def streamEncodeRows[F[_]](
      implicit
      p: Printer = Printer.default,
      mediaType: MediaType = MediaType.text.csv
  ): EntityEncoder[F, Stream[F, CSV.Row]] = {
    val contentTypeHeader = headers.`Content-Type`(mediaType)
    EntityEncoder.encodeBy(contentTypeHeader)(
      s =>
        Entity(
          s.through(encodeRows(p))
            .intersperse(p.rowSeparator)
            .through(text.utf8Encode)
        )
    )
  }

  def streamEncodeWrite[F[_], A: Write](
      implicit
      p: Printer = Printer.default,
      mediaType: MediaType = MediaType.text.csv
  ): EntityEncoder[F, Stream[F, A]] =
    streamEncodeRows[F].contramap(_.map(Write[A].write))

  def streamEncodeLabelledWrite[F[_], A: LabelledWrite](
      p: Printer = Printer.default,
      mediaType: MediaType = MediaType.text.csv
  ): EntityEncoder[F, Stream[F, A]] = {
    val contentTypeHeader = headers.`Content-Type`(mediaType)
    EntityEncoder.encodeBy(contentTypeHeader)(
      s =>
        Entity(
          s.through(writeLabelled(p))
            .intersperse(p.rowSeparator)
            .through(text.utf8Encode)
        )
    )
  }

  implicit def completeEntityDecoder[F[_]: Sync]: EntityDecoder[F, CSV.Complete] =
    new EntityDecoder[F, CSV.Complete] {
      def consumes: Set[MediaRange] = Set(MediaType.text.csv)
      def decode(msg: Media[F], strict: Boolean): DecodeResult[F, CSV.Complete] =
        cats.data.EitherT {
          msg.body
            .through(text.utf8Decode)
            .compile
            .foldMonoid
            .map(
              s =>
                _root_.io.chrisdavenport.cormorant.parser
                  .parseComplete(s)
                  .leftMap(parseError => org.http4s.MalformedMessageBodyFailure(parseError.reason))
            )
        }
    }

  implicit def rowsEntityDecoder[F[_]: Sync]: EntityDecoder[F, CSV.Rows] =
    new EntityDecoder[F, CSV.Rows] {
      def consumes: Set[MediaRange] = Set(MediaType.text.csv)
      def decode(msg: Media[F], strict: Boolean): DecodeResult[F, CSV.Rows] = cats.data.EitherT {
        msg.body
          .through(text.utf8Decode)
          .compile
          .foldMonoid
          .map(
            s =>
              _root_.io.chrisdavenport.cormorant.parser
                .parseRows(s)
                .leftMap(parseError => org.http4s.MalformedMessageBodyFailure(parseError.reason))
          )
      }
    }

  def streamingLabelledReadDecoder[F[_]: Sync, A: LabelledRead]: EntityDecoder[F, Stream[F, A]] =
    new EntityDecoder[F, Stream[F, A]] {
      def consumes: Set[MediaRange] = Set(MediaType.text.csv)
      def decode(msg: Media[F], strict: Boolean): DecodeResult[F, Stream[F, A]] =
        msg.body
          .through(text.utf8Decode)
          .through(readLabelled[F, A])
          .pure[DecodeResult[F, *]]
    }

  def streamingReadDecoder[F[_]: Sync, A: Read]: EntityDecoder[F, Stream[F, A]] =
    new EntityDecoder[F, Stream[F, A]] {
      def consumes: Set[MediaRange] = Set(MediaType.text.csv)
      def decode(msg: Media[F], strict: Boolean): DecodeResult[F, Stream[F, A]] =
        msg.body
          .through(text.utf8Decode)
          .through(readRows[F, A])
          .pure[DecodeResult[F, *]]
    }
}
