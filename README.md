Fezziwig
========

[![fezziwig Scala version support](https://index.scala-lang.org/guardian/fezziwig/fezziwig/latest-by-scala-version.svg?platform=jvm)](https://index.scala-lang.org/guardian/fezziwig/fezziwig)
[![Release](https://github.com/guardian/fezziwig/actions/workflows/release.yml/badge.svg)](https://github.com/guardian/fezziwig/actions/workflows/release.yml)

[Fezziwig](https://en.wikipedia.org/wiki/Mr._Fezziwig) is a library for compile time generation of [Circe](https://github.com/circe/circe) encoders/decoders for [Scrooge](https://twitter.github.io/scrooge/)-generated classes representing [Thrift](http://thrift.apache.org/) objects.

Installation
------------

```
libraryDependencies ++= Seq(
  "com.gu" %% "fezziwig" % "2.0.0"
)
```

Usage
-----

To use the library, import the macros and use [circe’s semiauto derivation](https://circe.github.io/circe/codecs/semiauto-derivation.html). ([Automatic derivation](https://circe.github.io/circe/codecs/auto-derivation.html) is no longer supported, as of v2.0.0.)

```
import io.circe.generic.semiauto.__
import com.gu.fezziwig.CirceScroogeMacros._
import com.gu.fezziwig.CirceScroogeWhiteboxMacros._

implicit val exampleStructEncoder: Encoder[ExampleStruct] = deriveEncoder
implicit val exampleStructDecoder: Decoder[ExampleStruct] = deriveDecoder
```

The generated decoders support accumulation of errors, e.g.

```
val result = exampleStructDecoder.accumulating(json.hcursor)
```
