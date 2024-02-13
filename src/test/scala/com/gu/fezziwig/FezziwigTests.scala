package com.gu.fezziwig

import com.gu.fezziwig.CirceScroogeMacros._
import diffson._
import diffson.circe._
import diffson.jsonpatch._
import diffson.jsonpatch.simplediff._
import io.circe.CursorOp.DownField
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless._
import io.circe.generic.encoding.DerivedAsObjectEncoder
import io.circe.generic.encoding.ReprAsObjectEncoder

class FezziwigTests extends AnyFlatSpec with Matchers  {

  implicit val outerEncoder: Encoder[OuterStruct] = {
    val fooWitness = Witness(Symbol("foo"))
    val innerWitness = Witness(Symbol("inner"))

    implicit def outerGeneric: LabelledGeneric.Aux[OuterStruct, labelled.FieldType[fooWitness.T, String] :: labelled.FieldType[innerWitness.T, Option[InnerStruct]] :: HNil] = {
      new LabelledGeneric[OuterStruct] {
        type Repr = labelled.FieldType[fooWitness.T, String] :: labelled.FieldType[innerWitness.T, Option[InnerStruct]] :: HNil

        def to(os: OuterStruct): Repr = {
          val foo: labelled.FieldType[fooWitness.T,String] = labelled.field(os.foo)
          val inner: labelled.FieldType[innerWitness.T, Option[InnerStruct]] = labelled.field(os.inner)
          foo :: inner :: HNil
        }
        def from(hlist: Repr): OuterStruct = hlist match {
          case foo :: inner :: HNil => OuterStruct(foo, inner)
        }
      }
    }

    deriveEncoder
  }

  implicit val innerEncoder: Encoder[InnerStruct] = {
    val outerWitness = Witness(Symbol("outer"))
    type InnerRepr = labelled.FieldType[outerWitness.T, Option[OuterStruct]] :: HNil

    implicit def innerGeneric: LabelledGeneric.Aux[InnerStruct, InnerRepr] = {
      new LabelledGeneric[InnerStruct] {
        type Repr = InnerRepr

        def to(is: InnerStruct): Repr = {
          val outer: labelled.FieldType[outerWitness.T, Option[OuterStruct]] = labelled.field(is.outer)
          outer :: HNil
        }
        def from(hlist: Repr): InnerStruct = hlist match {
          case outer :: HNil => InnerStruct(outer)
        }
      }
    }

    deriveEncoder
  }
  implicit val outerDecoder: Decoder[OuterStruct] = {
    val fooWitness = Witness(Symbol("foo"))
    val innerWitness = Witness(Symbol("inner"))

    implicit def outerGeneric: LabelledGeneric.Aux[OuterStruct, labelled.FieldType[fooWitness.T, String] :: labelled.FieldType[innerWitness.T, Option[InnerStruct]] :: HNil] = {
      new LabelledGeneric[OuterStruct] {
        type Repr = labelled.FieldType[fooWitness.T, String] :: labelled.FieldType[innerWitness.T, Option[InnerStruct]] :: HNil

        def to(os: OuterStruct): Repr = {
          val foo: labelled.FieldType[fooWitness.T,String] = labelled.field(os.foo)
          val inner: labelled.FieldType[innerWitness.T, Option[InnerStruct]] = labelled.field(os.inner)
          foo :: inner :: HNil
        }
        def from(hlist: Repr): OuterStruct = hlist match {
          case foo :: inner :: HNil => OuterStruct(foo, inner)
        }
      }
    }

    deriveDecoder
  }
  implicit val innerDecoder: Decoder[InnerStruct] = {
    val outerWitness = Witness(Symbol("outer"))
    type InnerRepr = labelled.FieldType[outerWitness.T, Option[OuterStruct]] :: HNil

    implicit def innerGeneric: LabelledGeneric.Aux[InnerStruct, InnerRepr] = {
      new LabelledGeneric[InnerStruct] {
        type Repr = InnerRepr

        def to(is: InnerStruct): Repr = {
          val outer: labelled.FieldType[outerWitness.T, Option[OuterStruct]] = labelled.field(is.outer)
          outer :: HNil
        }
        def from(hlist: Repr): InnerStruct = hlist match {
          case outer :: HNil => InnerStruct(outer)
        }
      }
    }

    deriveDecoder
  }

  it should "round-trip scrooge thrift models [outer]" in {
    val jsonString =
      """
        |{
        |  "foo" : "hello",
        |  "inner" : {
        |    "outer" : {
        |      "foo" : "oh",
        |      "inner" : null
        |    }
        |  }
        |}
      """.stripMargin
    // val decoded: OuterStruct = OuterStruct("hello", Some(InnerStruct(Some(OuterStruct("oh", None)))))

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded: OuterStruct = jsonBefore.as[OuterStruct].toOption.get
    println(s"Decoded is $decoded")

    val jsonAfter: Json = decoded.asJson

    val diffJ = diff[Json, JsonPatch[Json]](jsonBefore, jsonAfter)
    if (diffJ != JsonPatch(Nil)) println(s"${diffJ.toString}")
    diffJ should be(JsonPatch(Nil))
  }

  it should "round-trip scrooge thrift models" in {
    val jsonString =
      """
        |{
        |  "b": {
        |    "u": {
        |      "c": {
        |        "s": "test"
        |      }
        |    }
        |  },
        |  "foo": "test-foo",
        |  "bar": 3,
        |  "e": "ENUM_A",
        |  "intMap": {
        |    "k": [2]
        |  },
        |  "x": {
        |    "s": "x"
        |  }
        |}
      """.stripMargin

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded: StructA = jsonBefore.as[StructA].toOption.get

    val jsonAfter: Json = decoded.asJson

    val diffJ = diff[Json, JsonPatch[Json]](jsonBefore, jsonAfter)
    if (diffJ != JsonPatch(Nil)) println(s"${diffJ.toString}")
    diffJ should be(JsonPatch(Nil))
  }

  it should "accumulate errors" in {
    //In the following json, the fields 's', 'foo' and 'x' have incorrect types
    val expectedFailures = List(
      DecodingFailure("String", List(DownField("s"), DownField("c"), DownField("u"), DownField("b"))),
      DecodingFailure("String", List(DownField("foo"))),
      DecodingFailure("Expected an object", List(DownField("x")))
    )

    val jsonString =
      """
        |{
        |  "b": {
        |    "u": {
        |      "c": {
        |        "s": 1
        |      }
        |    }
        |  },
        |  "foo": 1,
        |  "bar": 3,
        |  "e": "ENUM_A",
        |  "intMap": {
        |    "k": [2]
        |  },
        |  "x": 2
        |}
      """.stripMargin

    val dec = Decoder[StructA]

    val jsonBefore: Json = parse(jsonString).toOption.get

    val result = dec.decodeAccumulating(jsonBefore.hcursor)

    result.isInvalid should be(true)
    result.swap.foreach(nel => {
      nel.toList.length should be(3)
      nel.toList should be(expectedFailures)
    })
  }
}
