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
import com.gu.fezziwig.Union1.UnknownUnionField

class FezziwigTests extends AnyFlatSpec with Matchers  {
  implicit val outerStructDecoder: Decoder[OuterStruct] = deriveDecoder
  implicit val outerStructEncoder: Encoder[OuterStruct] = deriveEncoder
  implicit val innerStructDecoder: Decoder[InnerStruct] = deriveDecoder
  implicit val innerStructEncoder: Encoder[InnerStruct] = deriveEncoder

  val cWitness = Witness(Symbol("c"))
  val dWitness = Witness(Symbol("d"))
  type Union1Repr = labelled.FieldType[cWitness.T, StructC] :+: labelled.FieldType[dWitness.T, StructD] :+: CNil
  implicit val union1GenericManual: LabelledGeneric.Aux[Union1, Union1Repr] = new LabelledGeneric[Union1] {
    type Repr = Union1Repr

    def to(union: Union1): Repr = {
      union match {
        case Union1.C(c) => Inl(labelled.field(c))
        case Union1.D(d) => Inr(Inl(labelled.field(d)))
        case UnknownUnionField(field) => throw new RuntimeException(s"Unknown union field in type Union1: $field")
      }
    }

    def from(repr: Repr): Union1 = {
      repr match {
        case Inl(c) => Union1.C(c)
        case Inr(Inl(d)) => Union1.D(d)
        case Inr(Inr(_: CNil)) => throw new RuntimeException("Encountered CNil while converting Union1 from ReprType")
      }

    }
  }

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

  implicit val decodeEnum: Decoder[Enum] = Decoder[String].map(value => {
    val withoutSeparators = value.filterNot(ch => ch == '_' || ch == '-')
    Enum.valueOf(withoutSeparators).getOrElse(Enum.EnumUnknownEnum.apply(-1))
  })
  implicit val encodeEnum: Encoder[Enum] = Encoder.instance {
    case Enum.EnumA => Json.fromString(Enum.EnumA.originalName)
    case Enum.EnumB => Json.fromString(Enum.EnumB.originalName)
    case _ => Json.Null
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

  it should "round-trip StructB (struct containing a union)" in {
    val jsonString =
      """
        |{
        |  "u" : {
        |    "c" : {
        |      "s" : "hello"
        |    }
        |  }
        |}
      """.stripMargin

    val jsonBefore: Json = parse(jsonString).toOption.get

    val decoded: StructB = jsonBefore.as[StructB].toOption.get
    // val decoded: StructB = StructB(Union1.C(StructC("hello")))
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
