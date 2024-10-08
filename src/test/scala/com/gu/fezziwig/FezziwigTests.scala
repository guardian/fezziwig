package com.gu.fezziwig

import com.gu.fezziwig.CirceScroogeMacros._
import com.gu.fezziwig.CirceScroogeWhiteboxMacros.{thriftStructLabelledGeneric, thriftUnionLabelledGeneric, tfieldLabelledGeneric}
import com.twitter.io.Buf
import com.twitter.scrooge._
import diffson._
import diffson.circe._
import diffson.jsonpatch._
import diffson.jsonpatch.simplediff._
import io.circe.CursorOp.DownField
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import org.apache.thrift.protocol._
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless._

class FezziwigTests extends AnyFlatSpec with Matchers  {
  implicit val union1Decoder: Decoder[Union1] = deriveDecoder
  implicit val union1Encoder: Encoder[Union1] = deriveEncoder

  implicit val structBDecoder: Decoder[StructB] = deriveDecoder
  implicit val structBEncoder: Encoder[StructB] = deriveEncoder

  implicit val structCDecoder: Decoder[StructC] = deriveDecoder
  implicit val structCEncoder: Encoder[StructC] = deriveEncoder

  implicit val structDDecoder: Decoder[StructD] = deriveDecoder
  implicit val structDEncoder: Encoder[StructD] = deriveEncoder

  implicit val structADecoder: Decoder[StructA] = deriveDecoder
  implicit val structAEncoder: Encoder[StructA] = deriveEncoder

  implicit val recTreeDecoder: Decoder[RecTree] = deriveDecoder
  implicit val recTreeEncoder: Encoder[RecTree] = deriveEncoder

  implicit val recListDecoder: Decoder[RecList] = deriveDecoder
  implicit val recListEncoder: Encoder[RecList] = deriveEncoder

  implicit val coRecDecoder: Decoder[CoRec] = deriveDecoder
  implicit val coRecEncoder: Encoder[CoRec] = deriveEncoder

  implicit val coRec2Decoder: Decoder[CoRec2] = deriveDecoder
  implicit val coRec2Encoder: Encoder[CoRec2] = deriveEncoder

  implicit val vectorTestDecoder: Decoder[VectorTest] = deriveDecoder
  implicit val vectorTestEncoder: Encoder[VectorTest] = deriveEncoder

  implicit val blockElementDecoder: Decoder[BlockElement] = deriveDecoder
  implicit val blockElementEncoder: Encoder[BlockElement] = deriveEncoder

  implicit val listElementFieldsDecoder: Decoder[ListElementFields] = deriveDecoder
  implicit val listElementFieldsEncoder: Encoder[ListElementFields] = deriveEncoder

  implicit val listItemDecoder: Decoder[ListItem] = deriveDecoder
  implicit val listItemEncoder: Encoder[ListItem] = deriveEncoder

  implicit val decodeDefaultTestStruct: Decoder[DefaultTestStruct] = deriveDecoder

  private def testRoundTrip[T](jsonString: String)(implicit decoder: Decoder[T], encoder: Encoder[T]) = {
    val jsonBefore: Json = parse(jsonString).toOption.get
    val decoded: T = jsonBefore.as[T].toOption.get
    val jsonAfter: Json = decoded.asJson.deepDropNullValues
    val diffJ = diff[Json, JsonPatch[Json]](jsonBefore, jsonAfter)
    diffJ should be(JsonPatch(Nil))
  }

  it should "round-trip recursive tree" in {
    testRoundTrip[RecTree](
      """
        |{
        |  "children": [
        |    { "item": 5, "children": [] },
        |    { "item": 4, "children": [] },
        |    { "item": 3, "children": [ { "item": 2, "children": [] } ] }
        |  ],
        |  "item": 1
        |}
        |""".stripMargin)
  }

  it should "round-trip recursive list" in {
    testRoundTrip[RecList](
      """
        |{
        |  "item": 1,
        |  "nextitem": {
        |    "item": 2,
        |    "nextitem": {
        |      "item": 3
        |    }
        |  }
        |}
        |""".stripMargin)
  }

  it should "round-trip co-recursive struct" in {
    testRoundTrip[CoRec](
      """
        |{
        |  "other": {
        |    "other": {
        |      "other": {
        |        "other": {
        |          "other": {
        |          }
        |        }
        |      }
        |    }
        |  }
        |}
        |""".stripMargin)
  }

  it should "round-trip recursive vector" in {
    testRoundTrip[VectorTest](
      """
        |{
        |  "lister": [
        |    {
        |      "item": 1,
        |      "nextitem": {
        |        "item": 2
        |      }
        |    },
        |    {
        |      "item": 3,
        |      "nextitem": {
        |        "item": 4
        |      }
        |    },
        |    {
        |      "item": 5,
        |      "nextitem": {
        |        "item": 6
        |      }
        |    }
        |  ]
        |}
        |""".stripMargin)
  }

  it should "round-trip scrooge recursive thrift models of the form A->Option[B]->List[C]->List[A]" in {
    testRoundTrip[BlockElement](
      """
        |{
        |  "listTypeData" : {
        |    "items" : [
        |      {
        |        "elements": [
        |          {},
        |          {}
        |        ]
        |      },
        |      {
        |        "elements": [
        |          {},
        |          {}
        |        ]
        |      }
        |    ]
        |  }
        |}
      """.stripMargin)
  }

  it should "round-trip StructB (struct containing a union)" in {
    testRoundTrip[StructB](
      """
        |{
        |  "u" : {
        |    "c" : {
        |      "s" : "hello"
        |    }
        |  }
        |}
      """.stripMargin)
  }

  it should "round-trip StructA (a more nested struct)" in {
    testRoundTrip[StructA](
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
      """.stripMargin)
  }

  it should "follow circe default behavior of serialising optional fields as nulls rather than missing" in {
    implicit val structWithOptionalEncoder: Encoder[StructWithOptional] = deriveEncoder
    val jsonAfter: Json = StructWithOptional().asJson
    jsonAfter.asObject.flatMap(_("optionalField")).value.isNull should be(true)
  }

  it should "accumulate errors" in {
    //In the following json, the fields 's', 'foo' and 'x' have incorrect types
    val expectedFailures = List(
      DecodingFailure("Got value '1' with wrong type, expecting string", List(DownField("s"), DownField("c"), DownField("u"), DownField("b"))),
      DecodingFailure("Got value '1' with wrong type, expecting string", List(DownField("foo"))),
      DecodingFailure("Missing required field", List(DownField("s"), DownField("x")))
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
      nel.toList.zip(expectedFailures).foreach {
        case (found, expected) => {
          found.message shouldBe expected.message
          found.history shouldBe expected.history
        }
      }
    })
  }

  it should "decode a missing optional field without default" in {
    val jsonString =
      """
        |{
        |  "second": 2,
        |  "third": 3,
        |  "fourth": 4,
        |  "fifth": 5,
        |  "sixth": 6,
        |  "seventh": [1,2]
        |}
      """.stripMargin

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded: Option[DefaultTestStruct] = jsonBefore.as[DefaultTestStruct].toOption

    decoded shouldBe Some(DefaultTestStruct(None, 2, 3, 4, 5, 6, List(1, 2)))
  }

  it should "decode a missing optional field with default" in {
    val jsonString =
      """
        |{
        |  "first": 1,
        |  "third": 3,
        |  "fourth": 4,
        |  "fifth": 5,
        |  "sixth": 6,
        |  "seventh": [1,2]
        |}
      """.stripMargin

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded = jsonBefore.as[DefaultTestStruct]

    decoded shouldBe Right(DefaultTestStruct(Some(1), 2, 3, 4, 5, 6, List(1, 2)))
  }

  it should "fail to decode a missing default-requiredness field without default" in {
    val jsonString =
      """
        |{
        |  "first": 1,
        |  "second": 2,
        |  "fourth": 4,
        |  "fifth": 5,
        |  "sixth": 6,
        |  "seventh": [1,2]
        |}
      """.stripMargin

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded: Decoder.Result[DefaultTestStruct] = jsonBefore.as[DefaultTestStruct]

    decoded.swap.map(_.message) shouldBe Right("Missing required field")
    decoded.swap.map(_.history) shouldBe Right(List(DownField("third")))
  }

  it should "decode a missing default-requiredness field with default" in {
    val jsonString =
      """
        |{
        |  "first": 1,
        |  "second": 2,
        |  "third": 3,
        |  "fifth": 5,
        |  "sixth": 6,
        |  "seventh": [1,2]
        |}
      """.stripMargin

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded = jsonBefore.as[DefaultTestStruct]

    decoded shouldBe Right(DefaultTestStruct(Some(1), 2, 3, 4, 5, 6, List(1, 2)))
  }

  it should "fail to decode a missing required field without default" in {
    val jsonString =
      """
        |{
        |  "first": 1,
        |  "second": 2,
        |  "third": 3,
        |  "fourth": 4,
        |  "sixth": 6,
        |  "seventh": [1, 2]
        |}
      """.stripMargin

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded: Decoder.Result[DefaultTestStruct] = jsonBefore.as[DefaultTestStruct]

    decoded.swap.map(_.message) shouldBe Right("Missing required field")
    decoded.swap.map(_.history) shouldBe Right(List(DownField("fifth")))
  }

  it should "decode a missing required field with default" in {
    val jsonString =
      """
        |{
        |  "first": 1,
        |  "second": 2,
        |  "third": 3,
        |  "fourth": 4,
        |  "fifth": 5,
        |  "seventh": [1,2]
        |}
      """.stripMargin

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded = jsonBefore.as[DefaultTestStruct]

    decoded shouldBe Right(DefaultTestStruct(Some(1), 2, 3, 4, 5, 6, List(1,2)))
  }

  it should "decode a missing required list field without default" in {
    val jsonString =
      """
        |{
        |  "first": 1,
        |  "second": 2,
        |  "third": 3,
        |  "fourth": 4,
        |  "fifth": 5,
        |  "sixth": 6
        |}
      """.stripMargin

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded: Decoder.Result[DefaultTestStruct] = jsonBefore.as[DefaultTestStruct]

    decoded shouldBe Right(DefaultTestStruct(Some(1), 2, 3, 4, 5, 6, List()))
  }

  it should "encode an UnknownUnionField successfully as an object (dropping field content)" in {
    val tfieldBlob = TFieldBlob(new TField("e", TType.STRUCT, 3), Buf.slowFromHexString("ff1e"))
    val x: Union1 = Union1.UnknownUnionField(tfieldBlob)

    val expectedString =
      """
        | {
        |   "__unknownUnionField": {
        |      "name": "e",
        |      "type": 12,
        |      "id": 3
        |   }
        | }
      """.stripMargin
    val expectedJson = parse(expectedString)

    Right(x.asJson) should be (expectedJson)

    val decoded: Either[DecodingFailure, Union1] = expectedJson.toOption.get.as[Union1]
    decoded should be (Right(Union1.UnknownUnionField(tfieldBlob.copy(content = Buf.Empty))))
  }

  it should "round-trip an UnknownUnionField successfully" in {
    testRoundTrip[Union1](
      """
        | {
        |   "__unknownUnionField": {
        |      "name": "e",
        |      "type": 12,
        |      "id": 3
        |   }
        | }
      """.stripMargin)
  }
}
