package com.gu.fezziwig

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import com.twitter.scrooge.{ThriftEnum, ThriftStruct, ThriftUnion}
import io.circe.{Decoder, Encoder}
import shapeless.{Lazy, |¬|}

/**
  * Macros for Circe encoding/decoding of various Scrooge-generated classes.
  */

object CirceScroogeMacros {
  type NotUnion[T] = |¬|[ThriftUnion]#λ[T]  //For telling the compiler not to use certain macros for thrift Unions

  /**
    * The macro bundle magic happens here.
    * Note - intellij doesn't like the references to methods in an uninstantiated class, but it does compile.
    */

  implicit def decodeThriftStruct[A <: ThriftStruct : NotUnion]: Decoder[A] = macro CirceScroogeMacrosImpl.decodeThriftStruct[A]
  implicit def decodeThriftEnum[A <: ThriftEnum]: Decoder[A] = macro CirceScroogeMacrosImpl.decodeThriftEnum[A]
  implicit def decodeThriftUnion[A <: ThriftUnion]: Decoder[A] = macro CirceScroogeMacrosImpl.decodeThriftUnion[A]

  implicit def encodeThriftStruct[A <: ThriftStruct : NotUnion]: Encoder[A] = macro CirceScroogeMacrosImpl.encodeThriftStruct[A]
  implicit def encodeThriftEnum[A <: ThriftEnum]: Encoder[A] = macro CirceScroogeMacrosImpl.encodeThriftEnum[A]
  implicit def encodeThriftUnion[A <: ThriftUnion]: Encoder[A] = macro CirceScroogeMacrosImpl.encodeThriftUnion[A]
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

  def decodeThriftStruct[A: c.WeakTypeTag](x: c.Tree): c.Tree = {
    val A = weakTypeOf[A]

    val apply = getApplyMethod(A)

    val params = apply.paramLists.head.zipWithIndex.map { case (param, i) =>
      val name = param.name
      val tpe = param.typeSignature
      val fresh = c.freshName(name)

      val decodeExpr = {
        val decodeParam =
          q"""cursor.downField(${name.toString}).success
            .map(x => x.as[$tpe](_root_.scala.Predef.implicitly[_root_.io.circe.Decoder[_root_.shapeless.Lazy[$tpe]]]))"""

        if (param.asTerm.isParamWithDefault) {
          /**
            * Fallback to param's default value if the JSON field is not present.
            * If the field is present but fails to decode then it's still an error.
            * Note: reverse-engineering the name of the default value because it's not easily available in the reflection API.
            */
          val defaultValue = A.companion.member(TermName("apply$default$" + (i + 1)))
          fq"""$fresh <- $decodeParam.getOrElse(_root_.scala.Right($defaultValue))"""
        } else {
          fq"""$fresh <- $decodeParam.getOrElse(_root_.scala.Left(_root_.io.circe.DecodingFailure("Missing field: " + ${name.toString}, cursor.history)))"""
        }
      }

      val accDecodeExpr = {
        /**
          * If we try to use the same tree twice, the compiler may blow up with the following helpful error message:
          *   `scala.reflect.internal.Types$TypeError: value <none> is not a member of com.gu.fezziwig.FezziwigTests`
          */
        val accDecodeParam =
          q"""cursor.downField(${name.toString}).success
            .map(x => _root_.scala.Predef.implicitly.decodeAccumulating(x))"""

        if (param.asTerm.isParamWithDefault) {
          val defaultValue = A.companion.member(TermName("apply$default$" + (i + 1)))
          q"""$accDecodeParam.getOrElse(_root_.cats.data.Validated.valid[_root_.cats.data.NonEmptyList[_root_.io.circe.DecodingFailure],$tpe]($defaultValue))"""
        } else {
          q"""$accDecodeParam.getOrElse(_root_.cats.data.Validated.invalidNel(_root_.io.circe.DecodingFailure("Missing field: " + ${name.toString}, cursor.history)))"""
        }
      }

      (fresh, decodeExpr, accDecodeExpr)
    }

    /**
      * Accumulate cats Validated results.
      * This is only supported for structs with <= 22 parameters.
      */
    val accumulating: Option[Tree] = {
      if (params.length <= 22) {

        val validationExpression = params.map(_._3) match {
          case a :: Nil => q"""$a.map($apply)"""  //Tuple1 is not supported
          case Nil => q"""$apply"""
          case exprs => q"""(..$exprs).mapN($apply)"""
        }

        Some(q"""
          override def decodeAccumulating(cursor: _root_.io.circe.HCursor): _root_.io.circe.Decoder.AccumulatingResult[$A] = {
            cursor.value.asObject.map(_ => $validationExpression)
              .getOrElse(_root_.cats.data.Validated.invalidNel(_root_.io.circe.DecodingFailure("Expected an object", cursor.history)))
          }
        """)
      } else {
        c.warning(c.enclosingPosition, s"Decoder for ThriftStruct ${A.typeSymbol.name.toString} will not support accumulated errors for nested types because it has more than 22 parameters.")
        None
      }
    }

    val r = q"""{
      new _root_.io.circe.Decoder[$A] {
        import cats.syntax.apply._
        import cats.syntax.either._
        def apply(cursor: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[$A] = {
          for (..${
            fq"""_ <- Either.fromOption(cursor.value.asObject, _root_.io.circe.DecodingFailure("Expected an object", cursor.history))""" +:
              params.map(_._2)
          }) yield $apply(..${params.map(_._1)})
        }

        ${accumulating.getOrElse(q"")}
      }
    }"""
    println(s"Made a decoder: $r")
    r
  }

  /**
    * Scrooge removes underscores from Thrift enum values.
    */
  def decodeThriftEnum[A: c.WeakTypeTag]: c.Tree = {
    val A = weakTypeOf[A]
    val typeName = A.typeSymbol.name.toString
    val valueOf = A.companion.member(TermName("valueOf"))
    val unknown = A.companion.member(TermName(s"EnumUnknown$typeName"))

    q"""
      _root_.io.circe.Decoder[String].map(value => {
        val withoutSeparators = value.filterNot(ch => ch == '_' || ch == '-')
        $valueOf(withoutSeparators).getOrElse($unknown.apply(-1))
      })
    """
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
  def decodeThriftUnion[A: c.WeakTypeTag]: c.Tree = {
    val A = weakTypeOf[A]

    val memberClasses: Iterable[Symbol] = getUnionMemberClasses(A)

    val decoderCases: (List[Tree],List[Tree]) = memberClasses.toList.foldLeft(List[Tree](), List[Tree]()) { (acc, memberClass) =>
      val applyMethod = getApplyMethod(memberClass.typeSignature)

      val param: Symbol = applyMethod.paramLists.headOption.flatMap(_.headOption).getOrElse(c.abort(c.enclosingPosition, s"Not a valid union class: could not find the member class' parameter (${A.typeSymbol.fullName})"))
      val paramType = param.typeSignature.dealias
      val paramName = param.name.toString

      val implicitDecoderForParam: c.Tree = getImplicitDecoder(paramType)

      val decExpr = {
        cq"""$paramName =>
            c.downField($paramName).success.map(_.as[$paramType]($implicitDecoderForParam).map($applyMethod))"""
      }

      val accDecExpr = {
        val decoderCopy = c.untypecheck(implicitDecoderForParam)

        cq"""$paramName =>
          c.downField($paramName).success.map(x => $decoderCopy.decodeAccumulating(x).map($applyMethod))
            .getOrElse(_root_.cats.data.Validated.invalidNel(_root_.io.circe.DecodingFailure("Unable to find " + $paramName, c.history)))"""
      }

      acc match { case (decs, accDecs) => (decs :+ decExpr, accDecs :+ accDecExpr) }
    }

    q"""{
      new _root_.io.circe.Decoder[$A] {
        def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[$A] = {
          val result: Option[_root_.io.circe.Decoder.Result[$A]] = c.keys.getOrElse(Nil).headOption.flatMap {
            case ..${decoderCases._1 ++ Seq(cq"""_ => _root_.scala.None""")}
          }
          result.getOrElse(Either.left(_root_.io.circe.DecodingFailure("Missing field under union: "+ ${A.typeSymbol.fullName}, c.history)))
        }

        override def decodeAccumulating(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.AccumulatingResult[$A] = {
          val result = c.keys.getOrElse(Nil).headOption.map {
            case ..${decoderCases._2 ++ Seq(cq"""_ => _root_.cats.data.Validated.invalidNel(_root_.io.circe.DecodingFailure("Unknown param in union: "+ ${A.typeSymbol.fullName}, c.history))""")}
          }
          result.getOrElse(_root_.cats.data.Validated.invalidNel(_root_.io.circe.DecodingFailure("Missing field under union: "+ ${A.typeSymbol.fullName}, c.history)))
        }
      }
    }"""
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

      if (isOption) {
        q"""val implicitEncoder = _root_.scala.Predef.implicitly[_root_.io.circe.Decoder[_root_.shapeless.Lazy[$tpe]]]; thrift.${name.toTermName}.map(${name.toString} -> implicitEncoder.apply(_))"""
      } else {
        q"""val implicitEncoder = _root_.scala.Predef.implicitly[_root_.io.circe.Decoder[_root_.shapeless.Lazy[$tpe]]]; _root_.scala.Some(${name.toString} -> implicitEncoder.apply(thrift.${name.toTermName}))"""
      }
    }

    val r = q"""{ _root_.io.circe.Encoder.instance((thrift: $A) => _root_.io.circe.Json.fromFields($pairs.flatten)) }"""
    println(s"Made an encoder: $r")
    r
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

  private def getImplicitDecoder(tpe: Type): c.Tree = {
    val decoderForType = appliedType(weakTypeOf[Decoder[_]].typeConstructor, tpe)

    val normalImplicitDecoder = c.inferImplicitValue(decoderForType)
    if (normalImplicitDecoder.nonEmpty) {
      // Found an implicit, no need to use Lazy.
      // We want to avoid Lazy as much as possible, because extracting its `.value` incurs a runtime cost.
      normalImplicitDecoder
    } else {
      // If we couldn't find an implicit, try again with shapeless `Lazy`.
      // This is to work around a problem with diverging implicits.
      // If you try to summon an implicit for heavily nested type, e.g. `Decoder[Option[Seq[String]]]` then the compiler sometimes gives up.
      // Wrapping with `Lazy` fixes this issue.
      val lazyDecoderForType = appliedType(weakTypeOf[Lazy[_]].typeConstructor, decoderForType)
      val implicitLazyDecoder = c.inferImplicitValue(lazyDecoderForType)
      if (implicitLazyDecoder.isEmpty) c.abort(c.enclosingPosition, s"Could not find an implicit Decoder[$tpe] even after resorting to Lazy")
      // Note: In theory we could use the `implicitLazyDecoder` that we just found, but... for some reason it crashes the compiler :(
      q"_root_.scala.Predef.implicitly[_root_.shapeless.Lazy[_root_.io.circe.Decoder[$tpe]]].value"
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
