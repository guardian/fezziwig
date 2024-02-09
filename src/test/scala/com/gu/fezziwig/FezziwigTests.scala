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
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.decoding.ReprDecoder

class FezziwigTests extends AnyFlatSpec with Matchers  {

  case class First(something: String, somethingElse: Option[Second])
  case class Second(first: First)

  val somethingWitness = Witness(Symbol("something"))
  val somethingElseWitness = Witness(Symbol("somethingElse"))
  type FirstRepr = shapeless.labelled.FieldType[somethingWitness.T,String] :: shapeless.labelled.FieldType[somethingElseWitness.T,Option[FezziwigTests.this.Second]] :: shapeless.HNil
  val firstWitness = Witness(Symbol("first"))
  type SecondRepr = FezziwigTests.this.First with shapeless.labelled.KeyTag[firstWitness.T,FezziwigTests.this.First] :: shapeless.HNil

  val firstGeneric: LabelledGeneric[First]{type Repr = FirstRepr} = LabelledGeneric[First]
  val firstAux: LabelledGeneric.Aux[First, FirstRepr] = firstGeneric
  val firstEncode: ReprAsObjectEncoder[FirstRepr] = ReprAsObjectEncoder.deriveReprAsObjectEncoder
  val firstDerived: DerivedAsObjectEncoder[First] = DerivedAsObjectEncoder.deriveEncoder(firstAux, Lazy(firstEncode))
  implicit val firstEncoder: Encoder[First] = deriveEncoder(Lazy(firstDerived))

  val secondGeneric: LabelledGeneric[Second]{type Repr = SecondRepr} = LabelledGeneric[Second]
  val secondAux: LabelledGeneric.Aux[Second, SecondRepr] = secondGeneric
  val secondEncode: ReprAsObjectEncoder[SecondRepr] = ReprAsObjectEncoder.deriveReprAsObjectEncoder
  val secondDerived: DerivedAsObjectEncoder[Second] = DerivedAsObjectEncoder.deriveEncoder(secondAux, Lazy(secondEncode))
  implicit val secondEncoder: Encoder[Second] = deriveEncoder(Lazy(secondDerived))

  val firstDecode: ReprDecoder[FirstRepr] = ReprDecoder.deriveReprDecoder
  val firstDerivedDecoder: DerivedDecoder[First] = DerivedDecoder.deriveDecoder(firstAux, firstDecode)
  implicit val firstDecoder: Decoder[First] = deriveDecoder(Lazy(firstDerivedDecoder))

  val secondDecode: ReprDecoder[SecondRepr] = ReprDecoder.deriveReprDecoder
  val secondDerivedDecoder: DerivedDecoder[Second] = DerivedDecoder.deriveDecoder(secondAux, secondDecode)
  implicit val secondDecoder: Decoder[Second] = deriveDecoder(Lazy(secondDerivedDecoder))

  it should "round-trip a recursive case class with circe’s semi-auto derivation" in {
    val first = First("hello", Some(Second(First("goodbye", None))))

    val json = first.asJson(firstEncoder)

    val firstAgain = json.as[First](firstDecoder).toOption.get

    first shouldBe firstAgain
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
