
inThisBuild(List(
  organization := "io.chrisdavenport",
  homepage := Some(url("https://github.com/ChristopherDavenport/cormorant")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "ChristopherDavenport",
      "Christopher Davenport",
      "chris@christopherdavenport.tech",
      url("https://www.github.com/ChristopherDavenport")
    )
  )
))


lazy val cormorant = project.in(file("."))
  .disablePlugins(MimaPlugin)
  .settings(skip in publish := true)
  .settings(commonSettings)
  //also aggregate docs once github4s is available for scala 2.13 and can be compiled properly
  .aggregate(core, generic, parser, refined, fs2, http4s)


val catsV = "2.1.1"
val catsEffectV = "2.1.4"
val catsEffectTestV = "0.4.2"
val shapelessV = "2.3.3"
val http4sV = "0.21.14"
val catsScalacheckV = "0.3.0"
val specs2V = "4.10.5"

lazy val core = project.in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    name := "cormorant-core"
  )

lazy val generic = project.in(file("modules/generic"))
  .settings(commonSettings)
  .dependsOn(core)
  .settings(
    name := "cormorant-generic",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3"
    )
  )

lazy val parser = project.in(file("modules/parser"))
  .settings(commonSettings)
  .dependsOn(core % "compile;test->test")
  .settings(
    name := "cormorant-parser",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "atto-core" % "0.8.0"
    )
  )

lazy val refined = project.in(file("modules/refined"))
  .settings(commonSettings)
  .dependsOn(core)
  .settings(
    name := "cormorant-refined",
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined" % "0.9.19",
    )
  )

lazy val fs2 = project.in(file("modules/fs2"))
  .settings(commonSettings)
  .dependsOn(core % "compile;test->test", parser)
  .settings(
    name := "cormorant-fs2",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "2.5.0",
      "co.fs2" %% "fs2-io"   % "2.5.0" % Test,
      "com.codecommit" %% "cats-effect-testing-specs2" % catsEffectTestV % Test
    )
  )

lazy val http4s = project.in(file("modules/http4s"))
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

lazy val docs = project.in(file("modules/docs"))
  .disablePlugins(MimaPlugin)
  .settings(skip in publish := true)
  .settings(commonSettings)
  .settings(
    //github4s is not yet available for scala 2.13
    scalaVersion := scala2_12,
    crossScalaVersions := Seq(scala2_12),
  )
  .dependsOn(core, generic, parser, refined, fs2, http4s)
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(TutPlugin)
  .settings{
    import microsites._
    Seq(
      micrositeName := "cormorant",
      micrositeDescription := "CSV Library for Scala",
      micrositeAuthor := "Christopher Davenport",
      micrositeGithubOwner := "ChristopherDavenport",
      micrositeGithubRepo := "cormorant",
      micrositeBaseUrl := "/cormorant",
      micrositeDocumentationUrl := "https://www.javadoc.io/doc/io.chrisdavenport/cormorant-core_2.12",
      micrositeFooterText := None,
      micrositeHighlightTheme := "atom-one-light",
      micrositePalette := Map(
        "brand-primary" -> "#3e5b95",
        "brand-secondary" -> "#294066",
        "brand-tertiary" -> "#2d5799",
        "gray-dark" -> "#49494B",
        "gray" -> "#7B7B7E",
        "gray-light" -> "#E5E5E6",
        "gray-lighter" -> "#F4F3F4",
        "white-color" -> "#FFFFFF"
      ),
      fork in tut := true,
      scalacOptions in Tut --= Seq(
        "-Xfatal-warnings",
        "-Ywarn-unused-import",
        "-Ywarn-numeric-widen",
        "-Ywarn-dead-code",
        "-Ywarn-unused:imports",
        "-Xlint:-missing-interpolator,_"
      ),
      libraryDependencies += "com.47deg" %% "github4s" % "0.20.1",
      micrositePushSiteWith := GitHub4s,
      micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
      micrositeExtraMdFiles := Map(
        file("CHANGELOG.md")        -> ExtraMdFileConfig("changelog.md", "page", Map("title" -> "changelog", "section" -> "changelog", "position" -> "100")),
        file("CODE_OF_CONDUCT.md")  -> ExtraMdFileConfig("code-of-conduct.md",   "page", Map("title" -> "code of conduct",   "section" -> "code of conduct",   "position" -> "101")),
        file("LICENSE")             -> ExtraMdFileConfig("license.md",   "page", Map("title" -> "license",   "section" -> "license",   "position" -> "102"))
      )
    )
  }


lazy val scala2_12 = "2.12.10"
lazy val scala2_13 = "2.13.1"

// General Settings
lazy val commonSettings = Seq(
  scalaVersion := scala2_12,
  crossScalaVersions := Seq(scalaVersion.value, scala2_13),

  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

  libraryDependencies ++= Seq(
    "org.typelevel"               %% "cats-core"                  % catsV,
    "org.typelevel"               %% "cats-effect"                % catsEffectV,
    "org.specs2"                  %% "specs2-core"                % specs2V       % Test,
    "org.specs2"                  %% "specs2-scalacheck"          % specs2V       % Test,
    "io.chrisdavenport"           %% "cats-scalacheck"            % catsScalacheckV % Test,
  )
)