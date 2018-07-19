---
layout: home

---
# comorant [![Build Status](https://travis-ci.com/ChristopherDavenport/cormorant.svg?branch=master)](https://travis-ci.com/ChristopherDavenport/cormorant) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/cormorant-core_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/cormorant-core_2.12)

## Project Goals

Cormorant is a CSV Library for Scala

## Quick Start

To use log4cats in an existing SBT project with Scala 2.11 or a later version, add the following dependencies to your
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