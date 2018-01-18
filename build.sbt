name := "jsonschema4s"

version := "0.1"

//scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.11.8", "2.12.4")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.reflections" % "reflections" % "0.9.11",
  "org.scalactic" %% "scalactic" % "3.0.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test")