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

class FezziwigTests extends AnyFlatSpec with Matchers  {

  implicit val decoder: Decoder[OuterStruct] = ???
  implicit val decoder: Decoder[InnerStruct] = ???


  implicit val encoder: Encoder[OuterStruct] = Encoder.instance(((thrift: OuterStruct) => Json.fromFields(List(Some("foo" -> (Encoder.encodeString.apply(thrift.foo))), thrift.inner.map(((x$5) => "inner" -> (encodeThriftStruct[InnerStruct](shapeless.`package`.nsub[InnerStruct, com.twitter.scrooge.ThriftUnion]).apply(x$5))))).flatten)))

  implicit val encoderInner: Encoder[InnerStruct] = Encoder.instance(((thrift: InnerStruct) => Json.fromFields(List(thrift.outer.map(((x) => "outer" -> (encodeThriftStruct[OuterStruct](shapeless.`package`.nsub[OuterStruct, com.twitter.scrooge.ThriftUnion]).apply(x))))).flatten)))

  implicit def outerGeneric[R](implicit
    gen: LabelledGeneric.Aux[InnerStruct, R],
  ): LabelledGeneric[OuterStruct] = new LabelledGeneric[OuterStruct] {
    type Repr = String :: Option[R] :: HNil

    def to(os: OuterStruct): Repr = os.foo :: os.inner.map(gen.to(_)) :: HNil
    def from(hlist: Repr): OuterStruct = hlist match {
      case foo :: inner :: HNil => OuterStruct(foo, inner.map(gen.from(_)))
    }
  }
  implicit def innerGeneric[R](implicit
    gen: LabelledGeneric.Aux[OuterStruct, R],
  ): LabelledGeneric[InnerStruct] = new LabelledGeneric[InnerStruct] {
    type Repr = Option[R] :: HNil

    def to(is: InnerStruct): Repr = is.outer.map(gen.to(_)) :: HNil
    def from(hlist: Repr): InnerStruct = hlist match {
      case outer :: HNil => InnerStruct(outer.map(gen.from(_)))
    }

  }

  implicit val outerAsObjectEncoder: DerivedAsObjectEncoder[OuterStruct] = implicitly[DerivedAsObjectEncoder[OuterStruct]]
  implicit val outerEncoder: Encoder[OuterStruct] = deriveEncoder
  implicit val innerAsObjectEncoder: DerivedAsObjectEncoder[InnerStruct] = implicitly[DerivedAsObjectEncoder[InnerStruct]]
  implicit val innerEncoder: Encoder[InnerStruct] = deriveEncoder

  it should "round-trip scrooge thrift models (outer)" in {
    val decoded: OuterStruct = OuterStruct("hello", Some(InnerStruct(Some(OuterStruct("oh", None)))))

    val jsonAfter: Json = decoded.asJson

    jsonAfter shouldBe("")
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
