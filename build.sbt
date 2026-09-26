ThisBuild / tlBaseVersion := "0.5" // current series x.y

ThisBuild / organization := "io.chrisdavenport"
ThisBuild / organizationName := "Christopher Davenport"
ThisBuild / startYear := Some(2018)
ThisBuild / licenses := Seq(License.MIT)
ThisBuild / developers := List(
  tlGitHubDev("ChristopherDavenport", "Christopher Davenport")
)

val Scala213 = "2.13.18"
// Scala 2 only: shapeless 2.x and atto have no Scala 3 build.
ThisBuild / crossScalaVersions := Seq(Scala213)
ThisBuild / scalaVersion := Scala213

val catsV = "2.13.0"
val catsEffectV = "3.7.1"
val fs2V = "3.14.0"
val http4sV = "0.23.37"
val catsScalacheckV = "0.5.0"
val munitV = "1.3.1"
val munitCatsEffectV = "2.2.1"
val scalacheckEffectV = "2.1.0"

lazy val root = project
  .in(file("."))
  .enablePlugins(NoPublishPlugin)
  .aggregate(core, generic, parser, refined, fs2, http4s)

lazy val core = project
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    name := "cormorant-core"
  )

lazy val generic = project
  .in(file("modules/generic"))
  .settings(commonSettings)
  .dependsOn(core)
  .settings(
    name := "cormorant-generic",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.9"
    )
  )

lazy val parser = project
  .in(file("modules/parser"))
  .settings(commonSettings)
  .dependsOn(core % "compile;test->test")
  .settings(
    name := "cormorant-parser",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "atto-core" % "0.8.0"
    )
  )

lazy val refined = project
  .in(file("modules/refined"))
  .settings(commonSettings)
  .dependsOn(core)
  .settings(
    name := "cormorant-refined",
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined" % "0.9.29"
    )
  )

lazy val fs2 = project
  .in(file("modules/fs2"))
  .settings(commonSettings)
  .dependsOn(core % "compile;test->test", parser)
  .settings(
    name := "cormorant-fs2",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % fs2V,
      "co.fs2" %% "fs2-io"   % fs2V % Test
    )
  )

lazy val http4s = project
  .in(file("modules/http4s"))
  .settings(commonSettings)
  .dependsOn(core % "compile;test->test", parser, fs2)
  .settings(
    name := "cormorant-http4s",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-core"   % http4sV,
      "org.http4s" %% "http4s-dsl"    % http4sV % Test,
      "org.http4s" %% "http4s-client" % http4sV % Test
    )
  )

// Replaces the sbt-microsites (Jekyll/Ruby) site. mdocIn resolves to the
// repo-root docs/ directory, which is where index.md now lives.
lazy val site = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core, generic, parser, refined, fs2, http4s)
  .settings(
    laikaTheme := tlSiteHelium.value.site
      .topNavigationBar(
        homeLink = laika.helium.config.IconLink.internal(laika.ast.Path.Root / "index.md", laika.helium.config.HeliumIcon.home)
      )
      .build
  )

// refined 0.9.29 pulls scala-xml 1.3.0 while the 2.12 compiler pulls 2.3.0.
// sbt 1.11's eviction check treats that as an error without an explicit scheme.
ThisBuild / libraryDependencySchemes +=
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

// General Settings
lazy val commonSettings = Seq(
  addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.13.4" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) => Seq("-Ypartial-unification")
    case _ => Nil
  }),
  testFrameworks += new TestFramework("munit.Framework"),
  libraryDependencies ++= Seq(
    "org.typelevel"     %% "cats-core"               % catsV,
    "org.typelevel"     %% "cats-effect"             % catsEffectV,
    "org.scalameta"     %% "munit"                   % munitV            % Test,
    "org.scalameta"     %% "munit-scalacheck"        % munitV            % Test,
    "org.typelevel"     %% "munit-cats-effect"     % munitCatsEffectV  % Test,
    "org.typelevel"     %% "scalacheck-effect-munit" % scalacheckEffectV % Test,
    "io.chrisdavenport" %% "cats-scalacheck"         % catsScalacheckV   % Test
  )
)
