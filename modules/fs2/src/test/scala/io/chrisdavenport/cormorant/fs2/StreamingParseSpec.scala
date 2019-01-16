package io.chrisdavenport.cormorant.fs2

import cats.effect.IO
import fs2.Stream
import io.chrisdavenport.cormorant._

class StreamingParseSpec extends CormorantSpec {

    "Streaming printer should" in {
      "row should round trip" in prop { a : CSV.Row => 
        Stream.emit[IO, CSV.Row](a)
          .through(encodeRows(Printer.default))
          .through(parseRows)
          .compile
          .toList
          .unsafeRunSync must_=== List(a)
      }.set(minTestsOk = 20, workers = 2)
    "rows should round trip" in prop { a: CSV.Rows => 
      val decoded = CSV.Rows(
        Stream.emits[IO, CSV.Row](a.rows)
        .through(encodeRows(Printer.default))
        .through(parseRows)
        .compile
        .toList
        .unsafeRunSync
      )
      decoded must_=== a
    }.set(minTestsOk = 20, workers = 2)

  }

}