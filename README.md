Fezziwig
========


[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.gu/fezziwig_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.gu/fezziwig_2.11) [![Build Status](https://travis-ci.org/guardian/fezziwig.svg?branch=master)](https://travis-ci.org/guardian/fezziwig)

[Fezziwig](https://en.wikipedia.org/wiki/Mr._Fezziwig) is a library for compile time generation of [Circe](https://github.com/circe/circe) encoders/decoders for [Scrooge](https://twitter.github.io/scrooge/)-generated classes representing [Thrift](http://thrift.apache.org/) objects.

Installation
------------
```
libraryDependencies ++= Seq(
  "com.gu" %% "fezziwig" % "0.3"
)
```

Usage
-----
```
import com.gu.fezziwig.CirceScrooge.Macros._
``` 

For `AccumulatingDecoder` instances, separate macros need to be invoked, e.g.:
```
import com.gu.fezziwig.CirceScrooge.AccumulatingMacros._
val decoder = AccumulatingDecoder[MyThriftStruct]
val result = decoder(json.hcursor)
```