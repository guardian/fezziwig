Fezziwig
========

[![fezziwig Scala version support](https://index.scala-lang.org/guardian/fezziwig/fezziwig/latest-by-scala-version.svg?platform=jvm)](https://index.scala-lang.org/guardian/fezziwig/fezziwig)

[Fezziwig](https://en.wikipedia.org/wiki/Mr._Fezziwig) is a library for compile time generation of [Circe](https://github.com/circe/circe) encoders/decoders for [Scrooge](https://twitter.github.io/scrooge/)-generated classes representing [Thrift](http://thrift.apache.org/) objects.

Installation
------------
```
libraryDependencies ++= Seq(
  "com.gu" %% "fezziwig" % "0.VERSIONHERE"
)
```

Usage
-----
```
import com.gu.fezziwig.CirceScroogeMacros._
```

The generated decoders support accumulation of errors, e.g.
```
import com.gu.fezziwig.CirceScroogeMacros._
val decoder = Decoder[MyThriftStruct]
val result = decoder.accumulating(json.hcursor)
```
