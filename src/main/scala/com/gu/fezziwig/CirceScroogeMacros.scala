package com.gu.fezziwig

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import com.twitter.scrooge.ThriftEnum
import io.circe.{Decoder, Encoder}

/**
  * Macros for Circe encoding/decoding of various Scrooge-generated classes.
  */

object CirceScroogeMacros {
  /**
    * The macro bundle magic happens here.
    * Note - intellij doesn't like the references to methods in an uninstantiated class, but it does compile.
    */

  implicit def decodeThriftEnum[A <: ThriftEnum]: Decoder[A] = macro CirceScroogeMacrosImpl.decodeThriftEnum[A]
  implicit def encodeThriftEnum[A <: ThriftEnum]: Encoder[A] = macro CirceScroogeMacrosImpl.encodeThriftEnum[A]
}

private class CirceScroogeMacrosImpl(val c: blackbox.Context) {
  import c.universe._

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
}
