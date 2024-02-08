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

  implicit val outerGeneric: LabelledGeneric[OuterStruct] = {
    val gen = LabelledGeneric[Option[InnerStruct]]

    new LabelledGeneric[OuterStruct] {
      val fooWitness = Witness(Symbol("foo"))
      val innerWitness = Witness(Symbol("inner"))

      type Repr =
        labelled.FieldType[fooWitness.T, String] :: labelled.FieldType[innerWitness.T, gen.Repr] :: HNil

      def to(os: OuterStruct): Repr = {
        val foo: labelled.FieldType[fooWitness.T,String] = labelled.field(os.foo)
        val inner: labelled.FieldType[innerWitness.T, gen.Repr] = labelled.field(gen.to(os.inner))
        foo :: inner :: HNil
      }
      def from(hlist: Repr): OuterStruct = hlist match {
        case foo :: inner :: HNil => OuterStruct(foo, gen.from(inner))
      }
    }
  }

  implicit val innerGeneric: LabelledGeneric[InnerStruct] = {
    val gen = LabelledGeneric[Option[OuterStruct]]

    new LabelledGeneric[InnerStruct] {
      val outerWitness = Witness(Symbol("outer"))
      type Repr = labelled.FieldType[outerWitness.T, gen.Repr] :: HNil

      def to(is: InnerStruct): Repr = {
        val outer: labelled.FieldType[outerWitness.T, gen.Repr] = labelled.field(gen.to(is.outer))
        outer :: HNil
      }
      def from(hlist: Repr): InnerStruct = hlist match {
        case outer :: HNil => InnerStruct(gen.from(outer))
      }
    }
  }

  val tupleGeneric = LabelledGeneric[Tuple2[String, Int]]
  val exampleGeneric = tupleGeneric.to(("hello", 1))
  case class Example(foo: String, bar: Boolean)
  val caseClassGeneric = LabelledGeneric[Example]
  val fooWitness = Witness(Symbol("foo"))
  val barWitness = Witness(Symbol("bar"))
  type ExampleRepr = labelled.FieldType[fooWitness.T, String] :: labelled.FieldType[barWitness.T, Boolean] :: HNil
  val exampleRecord: ExampleRepr = caseClassGeneric.to(Example("hello", true))
  // val testEncodeRecord: ReprAsObjectEncoder[labelled.FieldType[fooWitness.T, String] :: labelled.FieldType[barWitness.T, Boolean] :: HNil] = ReprAsObjectEncoder.deriveReprAsObjectEncoder
  val testEncodeRecord: ReprAsObjectEncoder[ExampleRepr] = implicitly
  val genAuxExample: LabelledGeneric.Aux[Example, ExampleRepr] = implicitly
  val exampleDerived: DerivedAsObjectEncoder[Example] = DerivedAsObjectEncoder.deriveEncoder(genAuxExample, testEncodeRecord)

  // val recordGeneric = LabelledGeneric[]
  // val exampleRecordGeneric = recordGeneric.to()
  val outerAux: LabelledGeneric.Aux[OuterStruct, outerGeneric.Repr] = outerGeneric
  val testEncodeWorks: ReprAsObjectEncoder[HNil] = ReprAsObjectEncoder.deriveReprAsObjectEncoder
  val hello = Witness(Symbol("hello"))
  val testEncodeFails: ReprAsObjectEncoder[labelled.FieldType[hello.T, String] :: HNil] = ReprAsObjectEncoder.deriveReprAsObjectEncoder
  val outerEncode: ReprAsObjectEncoder[outerGeneric.Repr] = ReprAsObjectEncoder.deriveReprAsObjectEncoder
  val outerDerived: DerivedAsObjectEncoder[OuterStruct] = DerivedAsObjectEncoder.deriveEncoder(outerAux, outerEncode)
  implicit val outerEncoder: Encoder[OuterStruct] = deriveEncoder(outerDerived)
  implicit val innerEncoder: Encoder[InnerStruct] = deriveEncoder

  it should "round-trip scrooge thrift models [outer]" in {
    val decoded: OuterStruct = OuterStruct("hello", Some(InnerStruct(Some(OuterStruct("oh", None)))))

    val jsonAfter: Json = outerEncoder(decoded)

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
