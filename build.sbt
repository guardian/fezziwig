import sbt.Keys._
import sbtrelease.ReleaseStateTransformations._

name := "fezziwig"
scalaVersion := "2.11.8"
organization := "com.gu"

val circeVersion = "0.7.0"

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
    </developers>
  )
publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := { _ => false }
crossPaths := false
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
  "org.apache.thrift" % "libthrift" % "0.9.1",
  "com.twitter" %% "scrooge-core" % "4.5.0",
  "io.circe" %% "circe-parser" % circeVersion % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "com.github.agourlay" %% "cornichon" % "0.10.4" % "test",
  "org.gnieh" %% "diffson-circe" % "2.1.1" % "test"
)

//For tests
scroogeThriftSourceFolder in Test := baseDirectory.value / "src/test/thrift"
unmanagedResourceDirectories in Test += { baseDirectory.value / "src/test/thrift" }
