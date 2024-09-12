import ReleaseTransformations.*
import sbtversionpolicy.withsbtrelease.ReleaseVersion

name := "fezziwig"
scalaVersion := "2.13.14"
crossScalaVersions := Seq("2.12.20", scalaVersion.value)
organization := "com.gu"

val circeVersion = "0.14.10"

licenses := Seq(License.Apache2)

releaseVersion := ReleaseVersion.fromAggregatedAssessedCompatibilityWithLatestRelease().value

releaseCrossBuild := true // true if you cross-build the project for multiple Scala versions

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "org.apache.thrift" % "libthrift" % "0.19.0",
  "com.twitter" %% "scrooge-core" % "21.8.0",
  "io.circe" %% "circe-parser" % circeVersion % "test",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.gnieh" %% "diffson-circe" % "4.6.0" % "test"
)

//For tests
Test / scroogeThriftSourceFolder := baseDirectory.value / "src/test/thrift"
Test / unmanagedResourceDirectories += { baseDirectory.value / "src/test/thrift" }
