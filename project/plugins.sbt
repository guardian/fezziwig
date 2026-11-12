addSbtPlugin("com.twitter" % "scrooge-sbt-plugin" % "21.8.0")

addSbtPlugin("com.github.sbt" % "sbt-release" % "1.4.0")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.11.3")
addSbtPlugin("ch.epfl.scala" % "sbt-version-policy" % "3.2.1")

// Patch the build tool graph without changing published library dependencies.
dependencyOverrides += "org.msgpack" % "msgpack-core" % "0.9.11"
dependencyOverrides += "org.apache.httpcomponents" % "httpclient" % "4.5.13"
