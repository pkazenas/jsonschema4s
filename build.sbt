name := "jsonschema4s"
organization := "pl.pkazenas"
version := "0.3.2"

crossScalaVersions := Seq("2.11.8", "2.12.4")
scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.reflections" % "reflections" % "0.9.11",
  "io.spray" %% "spray-json" % "1.3.3",
  "org.scalaz" %% "scalaz-core" % "7.2.18",
  "org.scalactic" %% "scalactic" % "3.0.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test")


// publish configuration

// POM settings for Sonatype
homepage := Some(url("https://github.com/pkazenas/jsonschema4s"))

scmInfo := Some(ScmInfo(url("https://github.com/pkazenas/jsonschema4s"), "git@github.com:pkazenas/jsonschema4s.git"))

developers :=
  List(
    Developer(
      "pkazenas",
      "Piotr Kazenas",
      "admin@pkazenas.pl",
      url("https://github.com/pkazenas")))

licenses += ("MIT", url("https://opensource.org/licenses/MIT"))

publishMavenStyle := true

// Add sonatype repository settings
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)