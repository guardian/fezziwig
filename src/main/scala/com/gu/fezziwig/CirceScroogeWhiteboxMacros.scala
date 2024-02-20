package com.gu.fezziwig

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import com.twitter.scrooge.{ThriftStruct, ThriftUnion}
import shapeless.{|¬|, LabelledGeneric, Lazy}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedAsObjectEncoder

object CirceScroogeWhiteboxMacros {
  type NotUnion[T] = |¬|[ThriftUnion]#λ[T]  //For telling the compiler not to use certain macros for thrift Unions

  implicit def thriftStructDecoder[A <: ThriftStruct : NotUnion](implicit
    derivedDecoder: Lazy[DerivedDecoder[A]]
  ): Decoder[A] = deriveDecoder[A](derivedDecoder)
  implicit def thriftStructEncoder[A <: ThriftStruct : NotUnion](implicit
    derivedEncoder: Lazy[DerivedAsObjectEncoder[A]]
  ): Encoder[A] = deriveEncoder[A](derivedEncoder)

  implicit def thriftUnionDecoder[A <: ThriftUnion](implicit
    derivedDecoder: Lazy[DerivedDecoder[A]]
  ): Decoder[A] = deriveDecoder[A](derivedDecoder)
  implicit def thriftUnionEncoder[A <: ThriftUnion](implicit
    derivedEncoder: Lazy[DerivedAsObjectEncoder[A]]
  ): Encoder[A] = deriveEncoder[A](derivedEncoder)

  implicit def thriftStructGeneric[A <: ThriftStruct : NotUnion, R]: LabelledGeneric.Aux[A, R] = macro CirceScroogeWhiteboxMacrosImpl.thriftStructGeneric[A]

  implicit def thriftUnionGeneric[A <: ThriftUnion, R]: LabelledGeneric.Aux[A, R] = macro CirceScroogeWhiteboxMacrosImpl.thriftUnionGeneric[A]

}

private class CirceScroogeWhiteboxMacrosImpl(val c: whitebox.Context) {
  import c.universe._

  /**
    * Provides a LabelledGeneric implicit for Thrift structs based on the
    * companion object’s `apply` method.
    *
    * Thrift structs are not a standard ADT of sealed traits and case classes,
    * which means circe’s derivation doesn’t immediately work for them. However,
    * this implicit enables circe’s semi-auto derivation to work as normal: the
    * LabelledGeneric implicit converts between the Thrift struct and a hlist
    * representation that matches what shapeless would generate for a similar
    * case class, which circe knows how to generate an Encoder/Decoder for.
    *
    * =Example=
    *
    * For example, given this thrift definition
    *
    * {{{
    * struct OuterStruct {
    *   1: required string foo
    *   2: optional InnerStruct inner
    * }
    * }}}
    *
    * the produced implicit is roughly equivalent to:
    *
    * {{{
    * {
    *   import shapeless.{Witness, labelled, ::, HNil}
    *
    *   val fooWitness = Witness(Symbol("foo"))
    *   val innerWitness = Witness(Symbol("inner"))
    *   type OuterStructRepr = labelled.FieldType[fooWitness.T, String] :: labelled.FieldType[innerWitness.T, Option[Option[InnerStruct]]] :: HNil
    *
    *   new LabelledGeneric[OuterStruct] {
    *     type Repr = OuterStructRepr
    *
    *     def to(struct: OuterStruct): Repr = {
    *       val foo: labelled.FieldType[fooWitness.T, String] = labelled.field(struct.foo)
    *       val inner: labelled.FieldType[innerWitness.T, Option[Option[InnerStruct]]] = labelled.field(Some(struct.inner))
    *       foo :: inner :: HNil
    *     }
    *
    *     def from(hlist: Repr): OuterStruct = hlist match {
    *       case foo :: inner :: HNil => OuterStruct(foo, inner.getOrElse(None))
    *     }
    *   }
    * }
    * }}}
    *
    * =Scrooge Representation=
    *
    * Scrooge represents
    *
    * {{{
    * struct Foo {
    *   1: string a
    *   2: i32 b
    * }
    * }}}
    *
    * As a trait plus a companion object with a nested `Immutable` class.
    * Roughly:
    *
    * {{{
    * object Foo {
    *   def apply(a: String, b: Int) = new Immutable(a, b)
    *   class Immutable(val a: String, val b: Int) extends Foo
    * }
    *
    * trait Foo {
    *   def a: String
    *   def b: Int
    * }
    * }}}
    */
  def thriftStructGeneric[A: WeakTypeTag](x: Tree): Tree = {
    val A = weakTypeOf[A]
    val apply = getApplyMethod(A)
    val params = apply.paramLists.head
    val fieldNames: List[Tree] = params.zipWithIndex.map{case (p, i) =>
      if (p.asTerm.isParamWithDefault) {
        val defaultValue = A.companion.member(TermName("apply$default$" + (i + 1)))
        q"${TermName(p.name.toString())}.getOrElse(${defaultValue})"
      } else {
        q"""${TermName(p.name.toString())}"""
      }}
    val witnesses: List[Tree] = params.map(
      param => {
        val name = param.name
        val witnessName = TermName(s"${name.toString()}Witness")
        val symbolString = name.toString()
        q"""val ${witnessName} = _root_.shapeless.Witness(_root_.scala.Symbol(${symbolString}))"""
      }
    )
    val reprType: Tree = params.foldRight[Tree](tq"""_root_.shapeless.HNil""") { case (param, acc) =>
      val witnessName = TermName(s"${param.name}Witness")
      val fieldType: Tree = if (param.asTerm.isParamWithDefault) {
        tq"Option[${param.typeSignature}]"
      } else {
        q"${param.typeSignature}"
      }
      tq"""_root_.shapeless.::[_root_.shapeless.labelled.FieldType[${witnessName}.T, ${fieldType}], $acc]"""
    }
    val hlist = params.foldRight[Tree](q"""_root_.shapeless.HNil""") { case (param, acc) =>
      val paramName = TermName(param.name.toString())
      q"""_root_.shapeless.::(${paramName}, $acc)"""
    }
    // hlistPattern is identical to hlist but with the pq interpolator instead of q
    val hlistPattern = params.foldRight[Tree](pq"""_root_.shapeless.HNil""") { case (param, acc) =>
      val paramName = TermName(param.name.toString())
      pq"""_root_.shapeless.::(${paramName}, $acc)"""
    }
    val labelledFields = params.map(param => {
      val paramName = TermName(param.name.toString())
      val witnessName = TermName(s"${param.name.toString()}Witness")
      val (fieldExpr, fieldType): (Tree, Tree) = if (param.asTerm.isParamWithDefault) {
        (q"_root_.scala.Some(struct.${paramName})", tq"Option[${param.typeSignature}]")
      } else {
        (q"struct.${paramName}", q"${param.typeSignature}")
      }
      q"""val ${paramName}: _root_.shapeless.labelled.FieldType[${witnessName}.T, ${fieldType}] = _root_.shapeless.labelled.field(${fieldExpr})"""
      }
    )
    val labelledGeneric = q"""
    new _root_.shapeless.LabelledGeneric[$A] {
      type Repr = ${reprType}

      def to(struct: $A): Repr = {
        ..$labelledFields
        $hlist
      }

      def from(hlist: Repr): $A = hlist match {
        case $hlistPattern => $apply(..${fieldNames})
      }
    }"""
    q"""{
      ..$witnesses

      $labelledGeneric
    }"""
  }

  /**
    * Provides a LabelledGeneric implicit for Thrift unions, as
    * [[thriftStructGeneric]] does for structs.
    *
    * Given this thrift definition
    *
    * {{{
    * union Union1 {
    *   1: StructC c
    *   2: StructD d
    * }
    * }}}
    *
    * the produced implicit will be roughly equivalent to
    *
    * {{{
    * {
    *   import shapeless.{Witness, :+:, labelled, CNil, Inl, Inr}
    *
    *   val dWitness = Witness(Symbol("d"))
    *   val cWitness = Witness(Symbol("c"))
    *   type Union1Repr = labelled.FieldType[dWitness.T, StructD] :+: labelled.FieldType[cWitness.T, StructC] :+: CNil
    *
    *   new LabelledGeneric[Union1] {
    *     type Repr = Union1Repr
    *
    *     def to(union: Union1): Repr = union match {
    *       case Union1.D(x) => Inl(labelled.field(x))
    *       case Union1.C(x) => Inr(Inl(labelled.field(x)))
    *       case Union1.UnknownUnionField(_) => throw new RuntimeException("Unknown union field")
    *     }
    *
    *     def from(repr: Repr): Union1 = repr match {
    *       case Inl(d) => Union1.D(d)
    *       case Inr(Inl(c)) => Union1.C(c)
    *       case Inr(Inr((_: CNil))) => throw new RuntimeException("Encountered CNil while converting from ReprType")
    *     }
    *   }
    * }
    * }}}
    */
  def thriftUnionGeneric[A: WeakTypeTag]: Tree = {
    val A = weakTypeOf[A]
    val memberClasses: List[Symbol] = getUnionMemberClasses(A)
    val paramsWithClasses = memberClasses.map(memberClass => {
      val apply = getApplyMethod(memberClass.typeSignature)
      val param = apply.paramLists.headOption.flatMap(_.headOption).getOrElse(
        c.abort(c.enclosingPosition, s"Not a valid union class: could not find the member class' parameter (${A.typeSymbol.fullName})")
      )
      (param, memberClass)
    })
    val witnesses: List[Tree] = paramsWithClasses.map{
      case (param, memberClass) => {
        val name = param.name
        val witnessName = TermName(s"${name.toString()}Witness")
        val symbolString = name.toString()
        q"""val ${witnessName} = _root_.shapeless.Witness(_root_.scala.Symbol(${symbolString}))"""
      }
    }
    val reprType: Tree = paramsWithClasses.foldRight[Tree](tq"""_root_.shapeless.CNil""") { case ((param, _), acc) =>
      val witnessName = TermName(s"${param.name}Witness")
      tq"""_root_.shapeless.:+:[_root_.shapeless.labelled.FieldType[${witnessName}.T, ${param.typeSignature.dealias}], $acc]"""
    }
    def injectR(n: Int)(t: Tree): Tree = {
      if (n == 0) {
        t
      } else {
        q"_root_.shapeless.Inr(${injectR(n - 1)(t)})"
      }
    }
    def injectRPattern(n: Int)(t: Tree): Tree = {
      if (n == 0) {
        t
      } else {
        pq"_root_.shapeless.Inr(${injectR(n - 1)(t)})"
      }
    }
    val toCases = paramsWithClasses.zipWithIndex.map{case ((param, memberClass), i) => {
      val field = q"_root_.shapeless.Inl(_root_.shapeless.labelled.field(x))"
      cq"${memberClass.companion}(x) => ${injectR(i)(field)}"
    }}
    val unknownUnionFieldCase = cq"""${A.typeSymbol.companion}.UnknownUnionField(_) => throw new _root_.java.lang.RuntimeException("Unknown union field")"""

    val fromCases = paramsWithClasses.zipWithIndex.map{case ((param, memberClass), i) => {
      val paramName = TermName(param.name.toString())
      cq"${injectRPattern(i)(pq"_root_.shapeless.Inl(${paramName})")} => ${getApplyMethod(memberClass.typeSignature)}(${paramName})"
    }}
    val cNilCase = cq"""${injectR(paramsWithClasses.length)(pq"(_: _root_.shapeless.CNil)")} => throw new RuntimeException("Encountered CNil while converting from ReprType")"""

    val labelledGeneric = q"""
    new _root_.shapeless.LabelledGeneric[$A] {
      type Repr = ${reprType}

      def to(union: $A): Repr = union match {
        case ..${toCases ++ List(unknownUnionFieldCase)}
      }

      def from(repr: Repr): $A = repr match {
        case ..${fromCases ++ List(cNilCase)}
      }
    }"""
    q"""{
      ..$witnesses

      $labelledGeneric
    }"""
  }

  private def getApplyMethod(tpe: Type): MethodSymbol = {
    tpe.companion.member(TermName("apply")) match {
      case symbol if symbol.isMethod && symbol.asMethod.paramLists.size == 1 => symbol.asMethod
      case _ => c.abort(c.enclosingPosition, "Not a valid Scrooge class: could not find the companion object's apply method")
    }
  }

  private def getUnionMemberClasses(unionType: Type): List[Symbol] = {
    unionType.companion.members.filter { member =>
      if (member.isClass && member.name.toString != "UnknownUnionField") {
        member.asClass.baseClasses.contains(unionType.typeSymbol)
      } else false
    }.toList
  }
}
