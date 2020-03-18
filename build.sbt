name := """play-codecheck"""

version := "1.0-SNAPSHOT"

maintainer := "cay@horstmann.com"

scalaVersion := "2.12.8"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

libraryDependencies ++= Seq(
  guice,
  "com.amazonaws" % "aws-java-sdk" % "1.11.496"
)

// no api docs in dist
sources in (Compile, doc) := Seq.empty

publishArtifact in (Compile, packageDoc) := false
