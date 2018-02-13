import sbt.Keys._
import sbtrelease.ReleaseStateTransformations._

name := "fezziwig"
scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.11", scalaVersion.value)
organization := "com.gu"

val circeVersion = "0.9.0-M2"

publishTo :=
  Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging)

pomExtra := (
  <url>https://github.com/guardian/fezziwig</url>
    <scm>
      <connection>scm:git:git@github.com:guardian/fezziwig.git</connection>
      <developerConnection>scm:git:git@github.com:guardian/fezziwig.git</developerConnection>
      <url>git@github.com:guardian/fezziwig.git</url>
    </scm>
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
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)
resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "org.apache.thrift" % "libthrift" % "0.9.3",
  "com.twitter" %% "scrooge-core" % "4.20.0",
  "io.circe" %% "circe-parser" % circeVersion % "test",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  "com.github.agourlay" %% "cornichon" % "0.10.4" % "test",
  "org.gnieh" %% "diffson-circe" % "2.1.1" % "test"
)

//For tests
scroogeThriftSourceFolder in Test := baseDirectory.value / "src/test/thrift"
unmanagedResourceDirectories in Test += { baseDirectory.value / "src/test/thrift" }
