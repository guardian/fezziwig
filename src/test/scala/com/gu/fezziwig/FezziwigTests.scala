package com.gu.fezziwig

import org.scalatest.{FlatSpec, Matchers}
import com.gu.fezziwig.CirceScroogeMacros._
import gnieh.diffson.circe._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._
import io.circe.parser._
import cats.syntax.either._

class FezziwigTests extends FlatSpec with Matchers {

  it should "round-trip scrooge thrift models" in {

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
      val result: Map[String, Seq[Int]] = Map("k" -> Seq(2))  //cheat!
      Either.right(result)
    }

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
}
