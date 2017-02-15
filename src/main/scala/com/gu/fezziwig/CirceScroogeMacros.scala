package com.gu.fezziwig

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import com.twitter.scrooge.{ThriftEnum, ThriftStruct, ThriftUnion}
import io.circe.{AccumulatingDecoder, Decoder, Encoder}
import shapeless.{Lazy, |¬|}

/**
  * Macros for Circe encoding/decoding of various Scrooge-generated classes.
  */

object CirceScrooge {
  type NotUnion[T] = |¬|[ThriftUnion]#λ[T]  //For telling the compiler not to use certain macros for thrift Unions

  /**
    * The macro bundle magic happens here.
    * Note - intellij doesn't like the references to methods in an uninstantiated class, but it does compile.
    */

  object Macros {
    implicit def decodeThriftStruct[A <: ThriftStruct : NotUnion]: Decoder[A] = macro CirceScroogeMacrosImpl.decodeThriftStructStandard[A]
    implicit def decodeThriftEnum[A <: ThriftEnum]: Decoder[A] = macro CirceScroogeMacrosImpl.decodeThriftEnumStandard[A]
    implicit def decodeThriftUnion[A <: ThriftUnion]: Decoder[A] = macro CirceScroogeMacrosImpl.decodeThriftUnionStandard[A]

    implicit def encodeThriftStruct[A <: ThriftStruct : NotUnion]: Encoder[A] = macro CirceScroogeMacrosImpl.encodeThriftStruct[A]
    implicit def encodeThriftEnum[A <: ThriftEnum]: Encoder[A] = macro CirceScroogeMacrosImpl.encodeThriftEnum[A]
    implicit def encodeThriftUnion[A <: ThriftUnion]: Encoder[A] = macro CirceScroogeMacrosImpl.encodeThriftUnion[A]
  }

  object AccumulatingMacros {
    implicit def decodeThriftStruct[A <: ThriftStruct : NotUnion]: AccumulatingDecoder[A] = macro CirceScroogeMacrosImpl.decodeThriftStructAccumulating[A]
    implicit def decodeThriftEnum[A <: ThriftEnum]: AccumulatingDecoder[A] = macro CirceScroogeMacrosImpl.decodeThriftEnumAccumulating[A]
    implicit def decodeThriftUnion[A <: ThriftUnion]: AccumulatingDecoder[A] = macro CirceScroogeMacrosImpl.decodeThriftUnionAccumulating[A]
  }
}

private class CirceScroogeMacrosImpl(val c: blackbox.Context) {
  import c.universe._

  /**
    * Macro to provide custom decoding of Thrift structs using the companion object's `apply` method.
    *
    * The macro is needed because Scrooge generates non-sealed traits.
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
  def decodeThriftStructStandard[A: c.WeakTypeTag](x: c.Tree): c.Tree = decodeThriftStruct(x, false)
  def decodeThriftStructAccumulating[A: c.WeakTypeTag](x: c.Tree): c.Tree = decodeThriftStruct(x, true)

  private def decodeThriftStruct[A: c.WeakTypeTag](x: c.Tree, accumulating: Boolean): c.Tree = {
    val A = weakTypeOf[A]

    val apply = getApplyMethod(A)

    val params = apply.paramLists.head.zipWithIndex.map { case (param, i) =>
      val name = param.name
      val tpe = param.typeSignature
      val fresh = c.freshName(name)

      val implicitDecoder: c.Tree = getImplicitDecoder(tpe, accumulating)

      // Note: we don't simply call `cursor.get[$tpe](...)` because we want to avoid allocating HistoryOp instances.
      // See https://github.com/travisbrown/circe/issues/329 for details.
      val decodeParam = {
        if (accumulating) {
          q"""cursor.downField(${name.toString}).success
           .map(x => $implicitDecoder(x)).getOrElse(_root_.cats.data.Validated.invalidNel(DecodingFailure("Attempt to decode value on failed cursor", cursor.history)))"""
        } else {
          q"""cursor.downField(${name.toString}).success
           .map(x => x.as[$tpe]($implicitDecoder)).getOrElse(_root_.scala.Left(_root_.io.circe.DecodingFailure("Missing field: " + ${name.toString}, Nil)))"""
        }
      }

      val decodeExpr = {
        if (param.asTerm.isParamWithDefault) {
          // Fallback to param's default value if the JSON field is not present (or can't be decoded for some other reason).
          // Note: reverse-engineering the name of the default value because it's not easily available in the reflection API.
          val defaultValue = A.companion.member(TermName("apply$default$" + (i + 1)))

          if (accumulating)
            q"""$decodeParam.orElse(_root_.cats.data.Validated.valid($defaultValue))"""
          else
            fq"""$fresh <- $decodeParam.orElse(_root_.scala.Right($defaultValue))"""
        } else {
          if (accumulating)
            q"""$decodeParam"""
          else
            fq"""$fresh <- $decodeParam"""
        }
      }
      (fresh, decodeExpr)
    }

    if (accumulating) {
      /**
        * Accumulate cats Validated results using cartesian |@| operator, e.g.
        * (expr1 |@| expr2 |@| ...) map (apply)
        */
      val validation: Tree = params.map(_._2).reduce { (acc: Tree, expr: Tree) =>
        q"""$acc.|@|($expr)"""
      }

      q"""{
        import cats.syntax.cartesian._
        _root_.io.circe.AccumulatingDecoder.instance { (cursor: _root_.io.circe.HCursor) =>
          ($validation) map ($apply)
        }
      }"""

    } else {
      //Use a for-comprehension on the Either results
      q"""{
        _root_.io.circe.Decoder.instance((cursor: _root_.io.circe.HCursor) => for (..${params.map(_._2)}) yield $apply(..${params.map(_._1)}))
      }"""
    }
  }

  /**
    * Scrooge removes underscores from Thrift enum values.
    */
  def decodeThriftEnumStandard[A: c.WeakTypeTag]: c.Tree = decodeThriftEnum(false)
  def decodeThriftEnumAccumulating[A: c.WeakTypeTag]: c.Tree = decodeThriftEnum(true)

  private def decodeThriftEnum[A: c.WeakTypeTag](accumulating: Boolean): c.Tree = {
    val A = weakTypeOf[A]
    val typeName = A.typeSymbol.name.toString
    val valueOf = A.companion.member(TermName("valueOf"))
    val unknown = A.companion.member(TermName(s"EnumUnknown$typeName"))

    val tree =
      q"""
        _root_.io.circe.Decoder[String].map(value => {
          val withoutSeparators = _root_.org.apache.commons.lang3.StringUtils.replaceChars(value, "-_", "")
          $valueOf(withoutSeparators).getOrElse($unknown.apply(-1))
        })
      """

    if (accumulating) q"""$tree.accumulating"""
    else tree
  }

  /**
    * Macro to produce Decoders for ThriftUnion types.
    *
    * A ThriftUnion, U, has a companion object which defines a case class extending U for each member of the union.
    * Each case class takes a single parameter, whose type is an alias for the actual type contained by that member.
    *
    * E.g. for the following thrift definition:
    * {{{
    *   union U {
    *     1: FooStruct foo
    *     2: BarStruct bar
    *   }
    *
    *   struct T {
    *     1: U union
    *   }
    * }}}
    *
    * We may expect the following JSON:
    * {{{
    *   {
    *     union: {
    *       foo: {
    *         ...
    *       }
    *     }
    *   }
    * }}}
    *
    * In the scrooge-generated code, for the member foo of union U, there exists a case class:
    *   {{{
    *   case class Foo(foo: FooAlias) extends U
    *   }}}
    * where FooAlias aliases FooStruct.
    * We need to match on the single JSON field and determine which member of the union is present.
    * So the decoder will contain a case statement for the member foo as follows:
    *   {{{
    *   case "foo" => c.downField("foo").flatMap(_.as[FooStruct](decoderForFooStruct).toOption).map(Foo)
    *   }}}
    *
    */
  def decodeThriftUnionStandard[A: c.WeakTypeTag]: c.Tree = decodeThriftUnion(false)
  def decodeThriftUnionAccumulating[A: c.WeakTypeTag]: c.Tree = decodeThriftUnion(true)

  private def decodeThriftUnion[A: c.WeakTypeTag](accumulating: Boolean): c.Tree = {
    val A = weakTypeOf[A]

    val memberClasses: Iterable[Symbol] = getUnionMemberClasses(A)

    val decoderCases: Iterable[Tree] = memberClasses.map { memberClass =>
      val applyMethod = getApplyMethod(memberClass.typeSignature)

      val param: Symbol = applyMethod.paramLists.headOption.flatMap(_.headOption).getOrElse(c.abort(c.enclosingPosition, s"Not a valid union class: could not find the member class' parameter (${A.typeSymbol.fullName})"))
      val paramType = param.typeSignature.dealias
      val paramName = param.name.toString

      val implicitDecoderForParam: c.Tree = getImplicitDecoder(paramType, accumulating)

      if (accumulating) {
        cq"""$paramName =>
            c.downField($paramName).success.map(x => $implicitDecoderForParam(x).map($applyMethod))
              .getOrElse(_root_.cats.data.Validated.invalidNel(DecodingFailure("Attempt to decode value on failed cursor", c.history)))
          """
      } else {
        cq"""$paramName => c.downField($paramName).success.flatMap(_.as[$paramType]($implicitDecoderForParam).toOption).map($applyMethod)"""
      }
    }

    if (accumulating) {
      q"""
        _root_.io.circe.AccumulatingDecoder.instance {(c: _root_.io.circe.HCursor) =>
          val result = c.fields.getOrElse(Nil).headOption.map {
            case ..${decoderCases ++ Seq(cq"""_ => _root_.cats.data.Validated.invalidNel(DecodingFailure(${A.typeSymbol.fullName}, c.history))""")}
          }
          result.getOrElse(_root_.cats.data.Validated.invalidNel(DecodingFailure(${A.typeSymbol.fullName}, c.history)))
        }
      """
    } else {
      q"""
        _root_.io.circe.Decoder.instance {(c: _root_.io.circe.HCursor) =>
          val result = c.fields.getOrElse(Nil).headOption.flatMap {
            case ..${decoderCases ++ Seq(cq"""_ => _root_.scala.None""")}
          }
          Either.fromOption(result, DecodingFailure(${A.typeSymbol.fullName}, c.history))
        }
      """
    }

  }

  def encodeThriftStruct[A: c.WeakTypeTag](x: c.Tree): c.Tree = {
    val A = weakTypeOf[A]
    val apply = getApplyMethod(A)

    val pairs = apply.paramLists.head.map { param =>
      val name = param.name
      /**
        * We need to ignore any optional fields which are None, because they'll be included in the result as JNulls.
        */
      val (tpe, isOption) = param.typeSignature match {
        case TypeRef(_, sym, ps) if sym == typeOf[Option[_]].typeSymbol => (ps.head, true)
        case other => (other, false)
      }

      val implicitEncoder: c.Tree = getImplicitEncoder(tpe)

      if (isOption) q"""thrift.${name.toTermName}.map(${name.toString} -> $implicitEncoder.apply(_))"""
      else q"""_root_.scala.Some(${name.toString} -> $implicitEncoder.apply(thrift.${name.toTermName}))"""
    }

    q"""{ _root_.io.circe.Encoder.instance((thrift: $A) => _root_.io.circe.Json.fromFields($pairs.flatten)) }"""
  }

  /**
    * Assumes the JSON values match the original Thrift value definitions
    */
  def encodeThriftEnum[A: c.WeakTypeTag]: c.Tree = {
    val A = weakTypeOf[A]

    val valueSymbols: Iterable[Symbol] = A.companion.members.filter { member =>
      member.isModule && member.typeSignature.member(TermName("originalName")) != NoSymbol
    }

    val encoderCases = valueSymbols.map { valueSymbol: Symbol =>
      cq"""data: $valueSymbol => _root_.io.circe.Json.fromString($valueSymbol.originalName)"""
    }

    q"""
      _root_.io.circe.Encoder.instance {
        case ..${encoderCases ++ Seq(cq"""_ => _root_.io.circe.Json.Null""")}
      }
    """
  }

  def encodeThriftUnion[A: c.WeakTypeTag]: c.Tree = {
    val A = weakTypeOf[A]

    val memberClasses: Iterable[Symbol] = getUnionMemberClasses(A)

    val encoderCases: Iterable[Tree] = memberClasses.map { memberClass =>
      val applyMethod = getApplyMethod(memberClass.typeSignature)

      val param: Symbol = applyMethod.paramLists.headOption.flatMap(_.headOption).getOrElse(c.abort(c.enclosingPosition, s"Not a valid union class: could not find the member class' parameter (${A.typeSymbol.fullName})"))
      val paramType = param.typeSignature.dealias

      val implicitEncoderForParam: c.Tree = getImplicitEncoder(paramType)

      cq"""data: $memberClass => Json.obj(${param.name.toString} -> $implicitEncoderForParam.apply(data.${param.name.toTermName}))"""
    }

    q"""
      _root_.io.circe.Encoder.instance {
        case ..${encoderCases ++ Seq(cq"""_ => _root_.io.circe.Json.Null""")}
      }
    """
  }

  private def getApplyMethod(tpe: Type): MethodSymbol = {
    tpe.companion.member(TermName("apply")) match {
      case symbol if symbol.isMethod && symbol.asMethod.paramLists.size == 1 => symbol.asMethod
      case _ => c.abort(c.enclosingPosition, "Not a valid Scrooge class: could not find the companion object's apply method")
    }
  }

  private def getUnionMemberClasses(unionType: Type): Iterable[Symbol] = {
    unionType.companion.members.filter { member =>
      if (member.isClass && member.name.toString != "UnknownUnionField") {
        member.asClass.baseClasses.contains(unionType.typeSymbol)
      } else false
    }
  }

  private def isThrift(tpe: Type) = tpe <:< typeOf[ThriftStruct] || tpe <:< typeOf[ThriftUnion] || tpe <:< typeOf[ThriftEnum]

  private def getImplicitDecoder(tpe: Type, accumulating: Boolean): c.Tree = {
    val decoderForType = {
      if (accumulating && isThrift(tpe)) appliedType(weakTypeOf[AccumulatingDecoder[_]].typeConstructor, tpe)
      else appliedType(weakTypeOf[Decoder[_]].typeConstructor, tpe)
    }

    val normalImplicitDecoder = c.inferImplicitValue(decoderForType)
    if (normalImplicitDecoder.nonEmpty) {
      // Found an implicit, no need to use Lazy.
      // We want to avoid Lazy as much as possible, because extracting its `.value` incurs a runtime cost.
      if (accumulating) {
        if (isThrift(tpe)) normalImplicitDecoder else q"$normalImplicitDecoder.accumulating"
      } else normalImplicitDecoder
    } else {
      // If we couldn't find an implicit, try again with shapeless `Lazy`.
      // This is to work around a problem with diverging implicits.
      // If you try to summon an implicit for heavily nested type, e.g. `Decoder[Option[Seq[String]]]` then the compiler sometimes gives up.
      // Wrapping with `Lazy` fixes this issue.
      val lazyDecoderForType = appliedType(weakTypeOf[Lazy[_]].typeConstructor, decoderForType)
      val implicitLazyDecoder = c.inferImplicitValue(lazyDecoderForType)
      if (implicitLazyDecoder.isEmpty) c.abort(c.enclosingPosition, s"Could not find an implicit Decoder[$tpe] even after resorting to Lazy")

      // Note: In theory we could use the `implicitLazyDecoder` that we just found, but... for some reason it crashes the compiler :(
      if (accumulating) {
        if (isThrift(tpe)) q"_root_.scala.Predef.implicitly[_root_.shapeless.Lazy[_root_.io.circe.AccumulatingDecoder[$tpe]]].value"
        else q"_root_.scala.Predef.implicitly[_root_.shapeless.Lazy[_root_.io.circe.AccumulatingDecoder[$tpe]]].value.accumulating"
      } else {
        q"_root_.scala.Predef.implicitly[_root_.shapeless.Lazy[_root_.io.circe.Decoder[$tpe]]].value"
      }
    }
  }

  private def getImplicitEncoder(tpe: Type): c.Tree = {
    val encoderForType = appliedType(weakTypeOf[Encoder[_]].typeConstructor, tpe)
    val normalImplicitEncoder = c.inferImplicitValue(encoderForType)
    if (normalImplicitEncoder.nonEmpty) {
      normalImplicitEncoder
    } else {
      val lazyEncoderForType = appliedType(weakTypeOf[Lazy[_]].typeConstructor, encoderForType)
      val implicitLazyEncoder = c.inferImplicitValue(lazyEncoderForType)
      if (implicitLazyEncoder.isEmpty) c.abort(c.enclosingPosition, s"Could not find an implicit Encoder[$tpe] even after resorting to Lazy")

      q"_root_.scala.Predef.implicitly[_root_.shapeless.Lazy[_root_.io.circe.Encoder[$tpe]]].value"
    }
  }
}
