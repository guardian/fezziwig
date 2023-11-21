import sbt.Keys._

name := "fezziwig"
scalaVersion := "2.13.6"
crossScalaVersions := Seq("2.12.10", scalaVersion.value)
organization := "com.gu"

val circeVersion = "0.14.1"

publishMavenStyle := true
Test / publishArtifact := false
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

lazy val commonReleaseProcess = Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  setReleaseVersion,
  runClean,
  runTest,
  // For non cross-build projects, use releaseStepCommand("publishSigned")
  releaseStepCommandAndRemaining("+publishSigned"),
)

lazy val productionReleaseProcess = commonReleaseProcess ++ Seq[ReleaseStep](
  releaseStepCommand("sonatypeBundleRelease"),
)

lazy val snapshotReleaseProcess = commonReleaseProcess

releaseProcess := {
  sys.props.get("RELEASE_TYPE") match {
    case Some("production") => productionReleaseProcess
    case _ => snapshotReleaseProcess
  }
}

resolvers ++= Resolver.sonatypeOssRepos("snapshots")

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "org.apache.thrift" % "libthrift" % "0.19.0",
  "com.twitter" %% "scrooge-core" % "21.8.0",
  "io.circe" %% "circe-parser" % circeVersion % "test",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.gnieh" %% "diffson-circe" % "4.1.1" % "test"
)

//For tests
Test / scroogeThriftSourceFolder := baseDirectory.value / "src/test/thrift"
Test / unmanagedResourceDirectories += { baseDirectory.value / "src/test/thrift" }
