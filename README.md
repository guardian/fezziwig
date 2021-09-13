Fezziwig
========


[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.gu/fezziwig_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.gu/fezziwig_2.13) [![Build Status](https://travis-ci.org/guardian/fezziwig.svg?branch=master)](https://travis-ci.org/guardian/fezziwig)

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
