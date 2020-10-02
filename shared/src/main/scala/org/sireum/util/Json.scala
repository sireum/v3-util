/*
Copyright (c) 2015, Robby, Kansas State University
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.sireum.util


import scala.annotation.StaticAnnotation
import scala.annotation.meta.getter
import scala.language.existentials

object Json {

  @getter
  final class InternString extends StaticAnnotation

  @getter
  final class Extern extends StaticAnnotation

  import scala.language.implicitConversions

  implicit final def fromAnyVal(v: AnyVal): ujson.Value =
    v match {
      case true => ujson.True
      case false => ujson.False
      case v: Byte => ujson.Num(v)
      case v: Short => ujson.Num(v)
      case v: Char => ujson.Str(v.toString)
      case v: Int => ujson.Num(v)
      case v: Long => ujson.Str(v.toString)
      case v: Float => ujson.Num(v)
      case v: Double => ujson.Num(v)
    }

  implicit final def fromByteArray(a: Array[Byte]): ujson.Value =
    ujson.Str(a.map("%02X" format _).mkString)

  implicit final def fromStr(s: String): ujson.Str = ujson.Str(s)

  implicit final def fromSeq[T](c: CSeq[T])(
    implicit f: T => ujson.Value): ujson.Arr =
    ujson.Arr(c.map(f).toSeq: _*)

  implicit final def fromTuple2[T1, T2](t: (T1, T2))(
    implicit f1: T1 => ujson.Value, f2: T2 => ujson.Value): ujson.Arr =
    ujson.Arr(f1(t._1), f2(t._2))

  implicit final def fromTuple3[T1, T2, T3](t: (T1, T2, T3))(
    implicit
    f1: T1 => ujson.Value,
    f2: T2 => ujson.Value,
    f3: T3 => ujson.Value): ujson.Arr =
    ujson.Arr(f1(t._1), f2(t._2), f3(t._3))

  implicit final def fromOption[T](c: Option[T])(
    implicit f: T => ujson.Value): ujson.Arr =
    ujson.Arr(c.map(f).toSeq: _*)

  implicit final def fromLocationInfo(li: LocationInfo): ujson.Arr = {
    ujson.Arr(li.lineBegin, li.columnBegin, li.lineEnd, li.columnEnd,
      li.offset, li.length)
  }

  implicit final def toVector[T](v: ujson.Value)(
    implicit f: ujson.Value => T): Vector[T] =
    v match {
      case a: ujson.Arr =>
        var r = ivectorEmpty[T]
        for (v <- a.value) {
          r = r :+ f(v)
        }
        r
      case _ => sys.error("Unexpected ujson.Value for a sequence: " + v)
    }

  implicit final def toTuple2[T1, T2](v: ujson.Value)(
    implicit
    f1: ujson.Value => T1,
    f2: ujson.Value => T2): (T1, T2) =
    v match {
      case a: ujson.Arr =>
        (f1(a.value(0)), f2(a.value(1)))
      case _ => sys.error("Unexpected ujson.Value for a pair: " + v)
    }

  implicit final def toTuple3[T1, T2, T3](v: ujson.Value)(
    implicit
    f1: ujson.Value => T1,
    f2: ujson.Value => T2,
    f3: ujson.Value => T3): (T1, T2, T3) =
    v match {
      case a: ujson.Arr =>
        (f1(a.value(0)), f2(a.value(1)), f3(a.value(2)))
      case _ => sys.error("Unexpected ujson.Value for a triplet: " + v)
    }

  implicit final def toOption[T](v: ujson.Value)(
    implicit f: ujson.Value => T): Option[T] =
    v match {
      case a: ujson.Arr =>
        a.value.toSeq match {
          case Seq(value) => Some(f(value))
          case _ => None
        }
      case _ => sys.error("Unexpected ujson.Value for an option: " + v)
    }

  implicit final def toBoolean(v: ujson.Value): Boolean =
    v match {
      case ujson.True => true
      case ujson.False => false
      case _ => sys.error("Unexpected ujson.Value for a Boolean: " + v)
    }

  implicit final def toByte(v: ujson.Value): Byte =
    v match {
      case ujson.Num(d) => d.toByte
      case _ => sys.error("Unexpected ujson.Value for a Byte: " + v)
    }

  implicit final def toChar(v: ujson.Value): Char =
    v match {
      case ujson.Str(s) => s.charAt(0)
      case _ => sys.error("Unexpected ujson.Value for a Char: " + v)
    }

  implicit final def toShort(v: ujson.Value): Short =
    v match {
      case ujson.Num(d) => d.toShort
      case _ => sys.error("Unexpected ujson.Value for a Short: " + v)
    }

  implicit final def toInt(v: ujson.Value): Int =
    v match {
      case ujson.Num(d) => d.toInt
      case _ => sys.error("Unexpected ujson.Value for an Int: " + v)
    }

  implicit final def toLong(v: ujson.Value): Long =
    v match {
      case ujson.Str(s) => s.toString.toLong
      case _ => sys.error("Unexpected ujson.Value for a Long: " + v)
    }

  implicit final def toFloat(v: ujson.Value): Float =
    v match {
      case ujson.Num(d) => d.toFloat
      case _ => sys.error("Unexpected ujson.Value for a Float: " + v)
    }

  implicit final def toDouble(v: ujson.Value): Double =
    v match {
      case ujson.Num(d) => d
      case _ => sys.error("Unexpected ujson.Value for a Double: " + v)
    }

  implicit final def toByteArray(v: ujson.Value): Array[Byte] =
    v match {
      case ujson.Str(s) =>
        s.toString.replaceAll("[^0-9A-Fa-f]", "").toSeq.sliding(2, 2).map(_.unwrap).
          toArray.map(Integer.parseInt(_, 16).toByte)
      case _ => sys.error("Unexpected ujson.Value for an Array[Byte]: " + v)
    }

  final def toStrIntern(v: ujson.Value): String =
    v match {
      case ujson.Str(s) => s.toString.intern()
      case _ => sys.error("Unexpected ujson.Value for a String: " + v)
    }

  implicit final def toStr(v: ujson.Value): String =
    v match {
      case ujson.Str(s) => s.toString
      case _ => sys.error("Unexpected ujson.Value for a String: " + v)
    }

  implicit final def toLocationInfo(v: ujson.Value): Option[LocationInfo] = v match {
    case a: ujson.Arr =>
      if (a.value.isEmpty) None
      else {
        val Seq(lineBegin, columnBegin, lineEnd, columnEnd, offset, length) =
          a.value.toSeq.map(toInt)
        Some(LocationInfo(lineBegin , columnBegin, lineEnd, columnEnd, offset, length))
      }
    case _ => sys.error("Unexpected ujson.Value for a LocationInfo: " + v)
  }
}
