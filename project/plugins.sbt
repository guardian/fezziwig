resolvers += "twitter-repo" at "https://maven.twttr.com"
addSbtPlugin("com.twitter" % "scrooge-sbt-plugin" % "19.9.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.10")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.4")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")
