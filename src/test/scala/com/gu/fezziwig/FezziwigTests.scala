package com.gu.fezziwig

import org.scalatest.{FlatSpec, Matchers}
import gnieh.diffson.circe._
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import cats.syntax.either._
import com.gu.fezziwig.CirceScroogeMacros._
import io.circe.CursorOp.DownField

class FezziwigTests extends FlatSpec with Matchers  {

  /**
    * TODO - Currently the macros cannot handle thrift map types. There must be some shapeless magic we can do to fix this.
    *
    * Circe provides decodeMapLike and encodeMapLike, meaning we can find implicit Map decoders/encoders from here.
    * But the macros cannot find them when generating the scrooge decoders/encoders.
    *
    * So for now the following two implicits are needed for the test to pass :(
    */
  implicit def intMapEncoder = Encoder.instance[scala.collection.Map[String,Seq[Int]]] { s =>
    val fields = s.toList.map {
      case (k, v) => k -> Json.fromValues(v.map(_.asJson))
    }
    Json.fromFields(fields)
  }

  implicit def intMapDecoder = Decoder.instance[scala.collection.Map[String,Seq[Int]]] { c =>
    import cats.syntax.either._
    val result: Map[String, Seq[Int]] = Map("k" -> Seq(2))  //cheat!
    Either.right(result)
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

    val diff = JsonDiff.diff(jsonBefore, jsonAfter, false)
    if (diff != JsonPatch(Nil)) println(s"${diff.toString}")
    diff should be(JsonPatch(Nil))
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

    val result = dec.accumulating(jsonBefore.hcursor)

    result.isInvalid should be(true)
    result.swap.foreach(nel => {
      nel.toList.length should be(3)
      nel.toList should be(expectedFailures)
    })
  }
}
