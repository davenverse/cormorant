package io.chrisdavenport.cormorant.fs2

import fs2.Stream
import io.chrisdavenport.cormorant._

class StreamingParseSpec extends CormorantSpec {

    "Streaming printer should" in {

    "rows should round trip" in prop { a: CSV.Rows => 
      val encoded = Stream.emits(a.rows).through(encodeRows(Printer.default))
      val decoded = CSV.Rows(encoded.through(parseRows).toList)
      decoded must_=== a
    }.pendingUntilFixed


    // "complete should round trip" in prop {a: CSV.Complete => 
    //   import _root_.io.chrisdavenport.cormorant.implicits._
    //   val encoded = a.print(Printer.default)
    //   parseComplete(encoded) must_=== Either.right(a)
    // }

  }

}