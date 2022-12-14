name := "play-codecheck"

version := "1.0-SNAPSHOT"

maintainer := "cay@horstmann.com"

scalaVersion := "2.12.15"

javacOptions ++= Seq("-source", "11", "-target", "11")

lazy val root = (project in file(".")).enablePlugins(PlayJava)

libraryDependencies ++= Seq(
  guice,
  "com.amazonaws" % "aws-java-sdk" % "1.11.496",
  "net.oauth.core" % "oauth-provider" % "20100527",
  "oauth.signpost" % "signpost-core" % "1.2.1.2",
  "org.imsglobal" % "basiclti-util" % "1.1.2",   
  "com.google.cloud" % "google-cloud-secretmanager" % "1.4.0",
)

// no api docs in dist
sources in (Compile, doc) := Seq.empty

publishArtifact in (Compile, packageDoc) := false

// Eclipse
// Compile the project before generating Eclipse files, so
// that generated .scala or .class files for views and routes are present

// EclipseKeys.preTasks := Seq(compile in Compile, compile in Test)

// Java project. Don't expect Scala IDE
EclipseKeys.projectFlavor := EclipseProjectFlavor.Java

// Use .class files instead of generated .scala files for views and routes
//EclipseKeys.createSrc := EclipseCreateSrc.ValueSet(EclipseCreateSrc.ManagedClasses, EclipseCreateSrc.ManagedResources)
// gives horrible error message
// java.lang.NoSuchMethodError: 'scala.collection.immutable.Set sbt.internal.inc.Relations.products(java.io.File)'
// [error] 	at com.typesafe.sbteclipse.core.EclipsePlugin$.$anonfun$copyManagedClasses$2(EclipsePlugin.scala:117)


enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)

import com.typesafe.sbt.packager.docker._

dockerBaseImage := "docker.io/library/eclipse-temurin:11"
dockerEntrypoint := Seq("bin/play-codecheck", "-Dplay.server.pidfile.path=/dev/null", "-Dnashorn.args=--no-deprecation-warning")
dockerExposedPorts ++= Seq(9000, 9001)

import com.typesafe.sbt.packager.docker.DockerChmodType
dockerChmodType := DockerChmodType.UserGroupWriteExecute
lazy val fixDockerCommands = taskKey[Seq[com.typesafe.sbt.packager.docker.CmdLike]]("Fixes docker commands")

dockerCommands := {
  val n = dockerCommands.value.lastIndexWhere(p => p.toString.contains("USER"))
  val cmds = dockerCommands.value.splitAt(n)
  cmds._1 ++ List(
      Cmd("RUN", """["mkdir", "-pv", "/opt/codecheck/ext"]"""), 
      Cmd("RUN", """["chmod", "-R", "u=rwX,g=rwX", "/opt/codecheck"]"""),
    ) ++ cmds._2    
}
