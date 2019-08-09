---
layout: home

---
# cormorant [![Build Status](https://travis-ci.com/ChristopherDavenport/cormorant.svg?branch=master)](https://travis-ci.com/ChristopherDavenport/cormorant) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/cormorant-core_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/cormorant-core_2.12)

## Project Goals

Cormorant is a CSV Library for Scala

## Quick Start

To use cormorant in an existing SBT project with Scala 2.11 or a later version, add the following dependencies to your
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

```tut:silent
import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.parser._
import io.chrisdavenport.cormorant.implicits._
import cats.implicits._
import java.util.UUID
import java.time.Instant
```

Then some basic operations

```tut:book
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
