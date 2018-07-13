package io.chrisdavenport.cormorant.fs2

import fs2.Stream
import io.chrisdavenport.cormorant._

class StreamingParseSpec extends CormorantSpec {

    "Streaming printer should" in {
      "row should round trip" in prop { a : CSV.Row => 
        Stream.emit(a)
          .through(encodeRows(Printer.default))
          .through(parseRows)
          .toList must_=== List(a)
      }
    "rows should round trip" in prop { a: CSV.Rows => 
      val decoded = CSV.Rows(
        Stream.emits(a.rows)
        .through(encodeRows(Printer.default))
        .through(parseRows)
        .toList
      )
      decoded must_=== a
    }

  }

}