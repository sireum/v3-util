/*
Copyright (c) 2019, Robby, Kansas State University
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

// @formatter:off
// This file was auto-generated from org.sireum.util.Tag

package org.sireum.util


import org.sireum.util.Json._

object TagJson {
  import scala.language.implicitConversions

  implicit def fromTag(o: org.sireum.util.Tag): ujson.Obj =
    o match {
      case o: org.sireum.util.ErrorMessage =>
        ujson.Obj(
          (".class", ujson.Str("ErrorMessage")),
          ("kind", fromStr(o.kind)),
          ("message", fromStr(o.message))
        )
      case o: org.sireum.util.FileLocationInfoErrorMessage =>
        ujson.Obj(
          (".class", ujson.Str("FileLocationInfoErrorMessage")),
          ("kind", fromStr(o.kind)),
          ("uri", fromStr(o.uri)),
          ("lineBegin", fromAnyVal(o.lineBegin)),
          ("columnBegin", fromAnyVal(o.columnBegin)),
          ("lineEnd", fromAnyVal(o.lineEnd)),
          ("columnEnd", fromAnyVal(o.columnEnd)),
          ("offset", fromAnyVal(o.offset)),
          ("length", fromAnyVal(o.length)),
          ("message", fromStr(o.message))
        )
      case o: org.sireum.util.FileLocationInfoInfoMessage =>
        ujson.Obj(
          (".class", ujson.Str("FileLocationInfoInfoMessage")),
          ("kind", fromStr(o.kind)),
          ("uri", fromStr(o.uri)),
          ("lineBegin", fromAnyVal(o.lineBegin)),
          ("columnBegin", fromAnyVal(o.columnBegin)),
          ("lineEnd", fromAnyVal(o.lineEnd)),
          ("columnEnd", fromAnyVal(o.columnEnd)),
          ("offset", fromAnyVal(o.offset)),
          ("length", fromAnyVal(o.length)),
          ("message", fromStr(o.message))
        )
      case o: org.sireum.util.FileLocationInfoWarningMessage =>
        ujson.Obj(
          (".class", ujson.Str("FileLocationInfoWarningMessage")),
          ("kind", fromStr(o.kind)),
          ("uri", fromStr(o.uri)),
          ("lineBegin", fromAnyVal(o.lineBegin)),
          ("columnBegin", fromAnyVal(o.columnBegin)),
          ("lineEnd", fromAnyVal(o.lineEnd)),
          ("columnEnd", fromAnyVal(o.columnEnd)),
          ("offset", fromAnyVal(o.offset)),
          ("length", fromAnyVal(o.length)),
          ("message", fromStr(o.message))
        )
      case o: org.sireum.util.InfoMessage =>
        ujson.Obj(
          (".class", ujson.Str("InfoMessage")),
          ("kind", fromStr(o.kind)),
          ("message", fromStr(o.message))
        )
      case o: org.sireum.util.InternalErrorMessage =>
        ujson.Obj(
          (".class", ujson.Str("InternalErrorMessage")),
          ("kind", fromStr(o.kind)),
          ("message", fromStr(o.message))
        )
      case o: org.sireum.util.LocationInfo =>
        ujson.Obj(
          (".class", ujson.Str("LocationInfo")),
          ("lineBegin", fromAnyVal(o.lineBegin)),
          ("columnBegin", fromAnyVal(o.columnBegin)),
          ("lineEnd", fromAnyVal(o.lineEnd)),
          ("columnEnd", fromAnyVal(o.columnEnd)),
          ("offset", fromAnyVal(o.offset)),
          ("length", fromAnyVal(o.length))
        )
      case o: org.sireum.util.LocationInfoErrorMessage =>
        ujson.Obj(
          (".class", ujson.Str("LocationInfoErrorMessage")),
          ("kind", fromStr(o.kind)),
          ("lineBegin", fromAnyVal(o.lineBegin)),
          ("columnBegin", fromAnyVal(o.columnBegin)),
          ("lineEnd", fromAnyVal(o.lineEnd)),
          ("columnEnd", fromAnyVal(o.columnEnd)),
          ("offset", fromAnyVal(o.offset)),
          ("length", fromAnyVal(o.length)),
          ("message", fromStr(o.message))
        )
      case o: org.sireum.util.LocationInfoInfoMessage =>
        ujson.Obj(
          (".class", ujson.Str("LocationInfoInfoMessage")),
          ("kind", fromStr(o.kind)),
          ("lineBegin", fromAnyVal(o.lineBegin)),
          ("columnBegin", fromAnyVal(o.columnBegin)),
          ("lineEnd", fromAnyVal(o.lineEnd)),
          ("columnEnd", fromAnyVal(o.columnEnd)),
          ("offset", fromAnyVal(o.offset)),
          ("length", fromAnyVal(o.length)),
          ("message", fromStr(o.message))
        )
      case o: org.sireum.util.LocationInfoWarningMessage =>
        ujson.Obj(
          (".class", ujson.Str("LocationInfoWarningMessage")),
          ("kind", fromStr(o.kind)),
          ("lineBegin", fromAnyVal(o.lineBegin)),
          ("columnBegin", fromAnyVal(o.columnBegin)),
          ("lineEnd", fromAnyVal(o.lineEnd)),
          ("columnEnd", fromAnyVal(o.columnEnd)),
          ("offset", fromAnyVal(o.offset)),
          ("length", fromAnyVal(o.length)),
          ("message", fromStr(o.message))
        )
      case o: org.sireum.util.WarningMessage =>
        ujson.Obj(
          (".class", ujson.Str("WarningMessage")),
          ("kind", fromStr(o.kind)),
          ("message", fromStr(o.message))
        )
    }

  implicit def toTag[T <: org.sireum.util.Tag](v: ujson.Value): T =
    (v: @unchecked) match {
      case o: ujson.Obj =>
        (o.value.head._2.asInstanceOf[ujson.Str].value match {
           case "ErrorMessage" =>
             org.sireum.util.ErrorMessage(toStr(o.value.toSeq(1)._2), toStr(o.value.toSeq(2)._2))
           case "FileLocationInfoErrorMessage" =>
             org.sireum.util.FileLocationInfoErrorMessage(toStr(o.value.toSeq(1)._2), toStr(o.value.toSeq(2)._2), toInt(o.value.toSeq(3)._2), toInt(o.value.toSeq(4)._2), toInt(o.value.toSeq(5)._2), toInt(o.value.toSeq(6)._2), toInt(o.value.toSeq(7)._2), toInt(o.value.toSeq(8)._2), toStr(o.value.toSeq(9)._2))
           case "FileLocationInfoInfoMessage" =>
             org.sireum.util.FileLocationInfoInfoMessage(toStr(o.value.toSeq(1)._2), toStr(o.value.toSeq(2)._2), toInt(o.value.toSeq(3)._2), toInt(o.value.toSeq(4)._2), toInt(o.value.toSeq(5)._2), toInt(o.value.toSeq(6)._2), toInt(o.value.toSeq(7)._2), toInt(o.value.toSeq(8)._2), toStr(o.value.toSeq(9)._2))
           case "FileLocationInfoWarningMessage" =>
             org.sireum.util.FileLocationInfoWarningMessage(toStr(o.value.toSeq(1)._2), toStr(o.value.toSeq(2)._2), toInt(o.value.toSeq(3)._2), toInt(o.value.toSeq(4)._2), toInt(o.value.toSeq(5)._2), toInt(o.value.toSeq(6)._2), toInt(o.value.toSeq(7)._2), toInt(o.value.toSeq(8)._2), toStr(o.value.toSeq(9)._2))
           case "InfoMessage" =>
             org.sireum.util.InfoMessage(toStr(o.value.toSeq(1)._2), toStr(o.value.toSeq(2)._2))
           case "InternalErrorMessage" =>
             org.sireum.util.InternalErrorMessage(toStr(o.value.toSeq(1)._2), toStr(o.value.toSeq(2)._2))
           case "LocationInfo" =>
             org.sireum.util.LocationInfo(toInt(o.value.toSeq(1)._2), toInt(o.value.toSeq(2)._2), toInt(o.value.toSeq(3)._2), toInt(o.value.toSeq(4)._2), toInt(o.value.toSeq(5)._2), toInt(o.value.toSeq(6)._2))
           case "LocationInfoErrorMessage" =>
             org.sireum.util.LocationInfoErrorMessage(toStr(o.value.toSeq(1)._2), toInt(o.value.toSeq(2)._2), toInt(o.value.toSeq(3)._2), toInt(o.value.toSeq(4)._2), toInt(o.value.toSeq(5)._2), toInt(o.value.toSeq(6)._2), toInt(o.value.toSeq(7)._2), toStr(o.value.toSeq(8)._2))
           case "LocationInfoInfoMessage" =>
             org.sireum.util.LocationInfoInfoMessage(toStr(o.value.toSeq(1)._2), toInt(o.value.toSeq(2)._2), toInt(o.value.toSeq(3)._2), toInt(o.value.toSeq(4)._2), toInt(o.value.toSeq(5)._2), toInt(o.value.toSeq(6)._2), toInt(o.value.toSeq(7)._2), toStr(o.value.toSeq(8)._2))
           case "LocationInfoWarningMessage" =>
             org.sireum.util.LocationInfoWarningMessage(toStr(o.value.toSeq(1)._2), toInt(o.value.toSeq(2)._2), toInt(o.value.toSeq(3)._2), toInt(o.value.toSeq(4)._2), toInt(o.value.toSeq(5)._2), toInt(o.value.toSeq(6)._2), toInt(o.value.toSeq(7)._2), toStr(o.value.toSeq(8)._2))
           case "WarningMessage" =>
             org.sireum.util.WarningMessage(toStr(o.value.toSeq(1)._2), toStr(o.value.toSeq(2)._2))
         }).asInstanceOf[T]
    }
}