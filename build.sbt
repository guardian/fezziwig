import sbt.Keys._

name := "fezziwig"
scalaVersion := "2.13.1"
crossScalaVersions := Seq("2.12.10", scalaVersion.value)
organization := "com.gu"

val circeVersion = "0.12.1"

publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := { _ => false }
releasePublishArtifactsAction := PgpKeys.publishSigned.value
licenses := Seq("Apache v2" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))
homepage := Some(url("https://github.com/guardian/fezziwig"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/guardian/fezziwig"),
    "scm:git@github.com:guardian/fezziwig.git"
  )
)
developers := List(
  Developer(id = "tomrf1", name = "Tom Forbes", email = "", url = url("https://github.com/tomrf1")),
  Developer(id = "cb372", name = "Chris Birchall", email = "", url = url("https://github.com/cb372")),
  Developer(id = "mchv", name = "Mariot Chauvin", email = "", url = url("https://github.com/mchv")),
  Developer(id = "LATaylor-guardian", name = "Luke Taylor", email = "", url = url("https://github.com/LATaylor-guardian")),
  Developer(id = "annebyrne", name = "Anne Byrne", email = "", url = url("https://github.com/annebyrne"))
)

publishTo := sonatypePublishToBundle.value

import ReleaseTransformations._

releaseCrossBuild := true // true if you cross-build the project for multiple Scala versions
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  // For non cross-build projects, use releaseStepCommand("publishSigned")
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommand("sonatypeBundleRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges
)
resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "org.apache.thrift" % "libthrift" % "0.13.0",
  "com.twitter" %% "scrooge-core" % "19.11.0",
  "io.circe" %% "circe-parser" % circeVersion % "test",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.gnieh" %% "diffson-circe" % "4.0.0" % "test"
)

//For tests
scroogeThriftSourceFolder in Test := baseDirectory.value / "src/test/thrift"
unmanagedResourceDirectories in Test += { baseDirectory.value / "src/test/thrift" }
