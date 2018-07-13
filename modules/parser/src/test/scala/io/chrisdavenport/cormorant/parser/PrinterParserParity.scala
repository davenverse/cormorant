package io.chrisdavenport.cormorant.parser

import cats.implicits._
import io.chrisdavenport.cormorant._

class PrinterParserParity extends CormorantSpec {

  "Printer should round trip with parser" in {
    "field should round trip" in  prop {  a : CSV.Field =>
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseField(encoded) must_=== Either.right(a)
    }

    "row should round trip" in prop { a: CSV.Row => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseRow(encoded) must_=== Either.right(a)
    }

    "rows should round trip" in prop { a: CSV.Rows => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseRows(encoded) must_=== Either.right(a)
    }

    "header should round trip" in prop {a: CSV.Header => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseHeader(encoded) must_=== Either.right(a)
    }

    "headers should round trip" in prop {a: CSV.Headers => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseHeaders(encoded) must_=== Either.right(a)
    }

    "complete should round trip" in prop {a: CSV.Complete => 
      import _root_.io.chrisdavenport.cormorant.implicits._
      val encoded = a.print(Printer.default)
      parseComplete(encoded) must_=== Either.right(a)
    }

  }

}
