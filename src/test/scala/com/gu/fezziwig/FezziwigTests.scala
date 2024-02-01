package com.gu.fezziwig

import com.gu.fezziwig.CirceScroogeMacros._
import diffson._
import diffson.circe._
import diffson.jsonpatch._
import diffson.jsonpatch.simplediff._
import io.circe.CursorOp.DownField
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FezziwigTests extends AnyFlatSpec with Matchers  {

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

  it should "handle recursive type without crashing" in {
    val jsonString =
      """
        |{
        |  "foo": "outer-foo",
        |  "recursiveStruct": {
        |    "foo": "nested-foo"
        |  }
        |}
      """.stripMargin

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded: RecursiveStruct = jsonBefore.as[RecursiveStruct].toOption.get

    val jsonAfter: Json = decoded.asJson

    val diffJ = diff[Json, JsonPatch[Json]](jsonBefore, jsonAfter)
    if (diffJ != JsonPatch(Nil)) println(s"${diffJ.toString}")
    diffJ should be(JsonPatch(Nil))
  }
}
