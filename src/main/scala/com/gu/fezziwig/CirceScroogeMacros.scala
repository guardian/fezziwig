package com.gu.fezziwig

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import com.twitter.scrooge.{ThriftEnum, ThriftStruct, ThriftUnion}
import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, HCursor}
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

  implicit def decodeThriftStruct[A <: ThriftStruct]: Decoder[A] = macro CirceScroogeMacrosImpl.decodeThriftStruct[A]
  implicit def decodeThriftEnum[A <: ThriftEnum]: Decoder[A] = macro CirceScroogeMacrosImpl.decodeThriftEnum[A]
  //implicit def decodeThriftUnion[A <: ThriftUnion]: Decoder[A] = macro CirceScroogeMacrosImpl.decodeThriftUnion[A]

//  implicit def encodeThriftStruct[A <: ThriftStruct : NotUnion]: Encoder[A] = macro CirceScroogeMacrosImpl.encodeThriftStruct[A]
//  implicit def encodeThriftEnum[A <: ThriftEnum]: Encoder[A] = macro CirceScroogeMacrosImpl.encodeThriftEnum[A]
//  implicit def encodeThriftUnion[A <: ThriftUnion]: Encoder[A] = macro CirceScroogeMacrosImpl.encodeThriftUnion[A]
}

object CustomDecoders {
  def decodeThriftStructOption[A] = new Decoder[Option[A]] {
    override def apply(c: HCursor): Result[Option[A]] = Right(None)
  }
}

private class CirceScroogeMacrosImpl(val c: blackbox.Context) {
  import c.universe._

  case class DecoderDefinition(name: c.universe.TermName, optionName: c.universe.TermName, implementation: c.universe.Tree)

  val decoderCache = scala.collection.mutable.Map[Type, DecoderDefinition]()

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

    // TODO: probably this isn't a complete Option[T] implementation
    private def optionWrapped(A: Type, decoder: TermName) =
    q"""
        { new _root_.io.circe.Decoder[Option[$A]] {
          def apply(cursor: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Option[$A]] = {
            if (cursor.value.isNull) Right(None)
            else $decoder(cursor).map(Some.apply)
          }
        }
        }
       """

  def decodeThriftStruct[A: c.WeakTypeTag]: c.Tree = {
    val A = weakTypeOf[A]
    val decoderName = nameFromType(A)
    if (A <:< weakTypeOf[ThriftUnion])
      createThriftUnionDecoder(A, decoderName, nameFromTypeOption(A))
    else
      createThriftStructDecoder(A, decoderName, nameFromTypeOption(A))
    val dependencies = decoderCache.flatMap({ case (tpe, cacheEntry) =>
      List(
        q"""def ${cacheEntry.name}: Decoder[${tpe}] = ${cacheEntry.implementation}""",
        q"""def ${cacheEntry.optionName}: Decoder[Option[${tpe}]] = ${optionWrapped(tpe, cacheEntry.name)}"""
      )
    })
    q"""
       ..${dependencies}
       $decoderName
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


  private def nameFromType(tpe: Type) = {
    TermName(c.freshName(tpe.toString))
  }

  private def nameFromTypeOption(tpe: Type) =
    TermName(c.freshName("option" + tpe.toString))

  private def isOption(tpe: Type) =
    tpe.typeSymbol.fullName == "scala.Option"

  def createThriftStructDecoder(A: Type, decodeMe: c.universe.TermName, decodeOptionMe: c.universe.TermName): DecoderDefinition = {
    decoderCache(A) = DecoderDefinition(decodeMe, decodeOptionMe, q"???")

    val apply = getApplyMethod(A)

    case class ParamsData(forCompName: c.universe.TermName, decodeExpr: c.Tree, accDecodeExpr: c.Tree, decoderName: c.universe.TermName)

    val params: List[ParamsData] = apply.paramLists.head.zipWithIndex.map { case (param, i) =>
      val name = param.name
      val tpe = param.typeSignature
      val fresh = c.freshName(name)

      val decoderName: c.universe.TermName = getImplicitDecoder(tpe)

    val decodeExpr = {
        val decodeParam =
          q"""cursor.downField(${name.toString}).success
            .map(x => x.as[$tpe]($decoderName))"""

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
        val accDecodeParam =
          q"""cursor.downField(${name.toString}).success
            .map(x => $decoderName.decodeAccumulating(x))"""

        if (param.asTerm.isParamWithDefault) {
          val defaultValue = A.companion.member(TermName("apply$default$" + (i + 1)))
          q"""$accDecodeParam.getOrElse(_root_.cats.data.Validated.valid[_root_.cats.data.NonEmptyList[_root_.io.circe.DecodingFailure],$tpe]($defaultValue))"""
        } else {
          q"""$accDecodeParam.getOrElse(_root_.cats.data.Validated.invalidNel(_root_.io.circe.DecodingFailure("Missing field: " + ${name.toString}, cursor.history)))"""
        }
      }

      ParamsData(fresh.toTermName, decodeExpr, accDecodeExpr, decoderName)
    }

    /**
      * Accumulate cats Validated results.
      * This is only supported for structs with <= 22 parameters.
      */
    val accumulating: Option[Tree] = {
      if (params.length <= 22) {

        val validationExpression = params.map(_.accDecodeExpr) match {
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

    val implementation = q"""{
      new _root_.io.circe.Decoder[$A] {
        import cats.syntax.apply._
        import cats.syntax.either._
        def apply(cursor: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[$A] = {
          for (..${
      fq"""_ <- Either.fromOption(cursor.value.asObject, _root_.io.circe.DecodingFailure("Expected an object", cursor.history))""" +:
        params.map(_.decodeExpr)
    }) yield $apply(..${params.map(_.forCompName)})
        }

        ${accumulating.getOrElse(q"")}
      }
    }"""

    val entry = DecoderDefinition(decodeMe, decodeOptionMe, implementation)
    decoderCache(A) = entry
    entry
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

  def createThriftUnionDecoder(A: Type, decodeMe: c.universe.TermName, decodeOptionMe: c.universe.TermName): DecoderDefinition = {
    decoderCache(A) = DecoderDefinition(decodeMe, decodeOptionMe, q"???")
    val memberClasses: Iterable[Symbol] = getUnionMemberClasses(A)

    val decoderCases: (List[Tree],List[Tree]) = memberClasses.toList.foldLeft(List[Tree](), List[Tree]()) { (acc, memberClass) =>
      val applyMethod = getApplyMethod(memberClass.typeSignature)

      val param: Symbol = applyMethod.paramLists.headOption.flatMap(_.headOption).getOrElse(c.abort(c.enclosingPosition, s"Not a valid union class: could not find the member class' parameter (${A.typeSymbol.fullName})"))
      val paramType = param.typeSignature.dealias
      val paramName = param.name.toString

      val implicitDecoderForParam = getImplicitDecoder(paramType)

      val decExpr = {
        cq"""$paramName =>
            c.downField($paramName).success.map(_.as[$paramType]($implicitDecoderForParam).map($applyMethod))"""
      }

      val accDecExpr = {
        cq"""$paramName =>
          c.downField($paramName).success.map(x => $implicitDecoderForParam.decodeAccumulating(x).map($applyMethod))
            .getOrElse(_root_.cats.data.Validated.invalidNel(_root_.io.circe.DecodingFailure("Unable to find " + $paramName, c.history)))"""
      }

      acc match { case (decs, accDecs) => (decs :+ decExpr, accDecs :+ accDecExpr) }
    }

    val implementation = q"""{
      import cats.syntax.apply._
      import cats.syntax.either._

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

    val entry = DecoderDefinition(decodeMe, decodeOptionMe, implementation)
    decoderCache(A) = entry
    entry
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

      val encoder: c.Tree = {
        // if this is a recursive optional field then encode using `this`
        if (isOption && tpe == A) {
          q"this"
        } else {
          getImplicitEncoder(tpe)
        }
      }

      if (isOption) q"""thrift.${name.toTermName}.map(${name.toString} -> $encoder.apply(_))"""
      else q"""_root_.scala.Some(${name.toString} -> $encoder.apply(thrift.${name.toTermName}))"""
    }

    q"""{
       new _root_.io.circe.Encoder[$A] {
          def apply(thrift: $A): _root_.io.circe.Json = {
            _root_.io.circe.Json.fromFields($pairs.flatten)
          }
        }
     }"""
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

  private def getImplicitDecoder(tpe: Type): c.TermName = {

    val interestingType = if (isOption(tpe)) tpe.typeArgs.head else tpe
    val decoderName: c.universe.TermName = nameFromType(interestingType)
    val optionDecoderName = nameFromTypeOption(interestingType)

    val cachedValue = decoderCache.getOrElse(interestingType, {
      if (interestingType <:< weakTypeOf[ThriftStruct]) {
        if (interestingType <:< weakTypeOf[ThriftUnion])
          createThriftUnionDecoder(interestingType, decoderName, optionDecoderName)
        else
          createThriftStructDecoder(interestingType, decoderName, optionDecoderName)
      } else {
        val decoderForType = appliedType(weakTypeOf[Decoder[_]].typeConstructor, interestingType)

        val normalImplicitDecoder = c.inferImplicitValue(decoderForType)
        val implementation = if (normalImplicitDecoder.nonEmpty) {
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
          if (implicitLazyDecoder.isEmpty) c.abort(c.enclosingPosition, s"Could not find an implicit Decoder[$interestingType] even after resorting to Lazy")
          // Note: In theory we could use the `implicitLazyDecoder` that we just found, but... for some reason it crashes the compiler :(
          q"_root_.scala.Predef.implicitly[_root_.shapeless.Lazy[_root_.io.circe.Decoder[$interestingType]]].value"
        }

        val definition = DecoderDefinition(decoderName, optionDecoderName, implementation)
        decoderCache(interestingType) = definition
        definition
      }
    })
    if (isOption(tpe))
      cachedValue.optionName
    else
      cachedValue.name

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
