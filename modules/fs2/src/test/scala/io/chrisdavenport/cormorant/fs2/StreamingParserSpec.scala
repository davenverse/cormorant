package io.chrisdavenport.cormorant
package fs2

import cats.data.NonEmptyList
import cats.effect._
import munit.CatsEffectSuite
import io.chrisdavenport.cormorant._
import java.io.ByteArrayInputStream
import java.io.InputStream

class StreamingParserSpec extends CatsEffectSuite {

  def ruinDelims(str: String) = augmentString(str).flatMap {
    case '\n' => "\r\n"
    case c => c.toString
  }

  // https://github.com/ChristopherDavenport/cormorant/pull/84
  test("Streaming Parser parses a known value that did not work with streaming") {
    val x = """First Name,Last Name,Email
Larry,Bordowitz,larry@example.com
Anonymous,Hippopotamus,hippo@example.com"""
    val source = IO.pure(new ByteArrayInputStream(ruinDelims(x).getBytes): InputStream)
    _root_.fs2.io.readInputStream(
      source,
      chunkSize = 4,
    )
    .through(_root_.fs2.text.utf8Decode)
    .through(parseComplete[IO])
    .compile
    .toVector
    .map{ v =>
      val header = CSV.Headers(NonEmptyList.of(CSV.Header("First Name"), CSV.Header("Last Name"), CSV.Header("Email")))
      val row1 = CSV.Row(NonEmptyList.of(CSV.Field("Larry"), CSV.Field("Bordowitz"), CSV.Field("larry@example.com")))
      val row2 = CSV.Row(NonEmptyList.of(CSV.Field("Anonymous"), CSV.Field("Hippopotamus"), CSV.Field("hippo@example.com")))
      assertEquals(Vector(
        (header, row1),
        (header, row2)
      ), v)
    }
  }
}
