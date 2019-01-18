import sbt.Keys._
import sbtrelease.ReleaseStateTransformations._

name := "fezziwig"
scalaVersion := "2.12.8"
crossScalaVersions := Seq("2.11.12", scalaVersion.value)
organization := "com.gu"

val circeVersion = "0.11.1"

publishTo :=
  Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging)

pomExtra := (
    <developers>
      <developer>
        <id>tomrf1</id>
        <name>Tom Forbes</name>
        <url>https://github.com/tomrf1</url>
      </developer>
      <developer>
        <id>cb372</id>
        <name>Chris Birchall</name>
        <url>https://github.com/cb372</url>
      </developer>
      <developer>
        <id>mchv</id>
        <name>Mariot Chauvin</name>
        <url>https://github.com/mchv</url>
      </developer>
      <developer>
        <id>LATaylor-guardian</id>
        <name>Luke Taylor</name>
        <url>https://github.com/LATaylor-guardian</url>
      </developer>
      <developer>
        <id>annebyrne</id>
        <name>Anne Byrne</name>
        <url>https://github.com/annebyrne</url>
      </developer>
    </developers>
  )
publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := { _ => false }
releasePublishArtifactsAction := PgpKeys.publishSigned.value
licenses := Seq("Apache v2" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))
releaseProcess := Seq(
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts,
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeRelease"),
  pushChanges
)
resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "org.apache.thrift" % "libthrift" % "0.9.3",
  "com.twitter" %% "scrooge-core" % "4.18.0",
  "io.circe" %% "circe-parser" % circeVersion % "test",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  "com.github.agourlay" %% "cornichon" % "0.10.4" % "test",
  "org.gnieh" %% "diffson-circe" % "2.1.1" % "test"
)

//For tests
scroogeThriftSourceFolder in Test := baseDirectory.value / "src/test/thrift"
unmanagedResourceDirectories in Test += { baseDirectory.value / "src/test/thrift" }
