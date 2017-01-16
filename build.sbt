
name := "fezziwig"
scalaVersion := "2.11.8"

val circeVersion = "0.6.1"

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
scroogeThriftSourceFolder in Compile := baseDirectory.value / "src/test/thrift"
unmanagedResourceDirectories in Compile += { baseDirectory.value / "src/test/thrift" }
