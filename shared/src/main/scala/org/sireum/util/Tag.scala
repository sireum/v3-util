/*
 Copyright (c) 2016, Robby, Kansas State University
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

import upickle.default.{macroRW, ReadWriter => RW}


sealed trait Tag extends Product

object Tag {
  implicit def rw: RW[Tag] = RW.merge(KindTag.rw, UriTag.rw, LocationInfoTag.rw, SeverityTag.rw, MessageTag.rw)
}

sealed trait KindTag extends Tag {
  def kind: String
}

object KindTag {
  implicit def rw: RW[KindTag] = RW.merge(FileLocationInfoErrorMessage.rw,
    FileLocationInfoWarningMessage.rw, FileLocationInfoInfoMessage.rw, LocationInfoErrorMessage.rw,
    LocationInfoWarningMessage.rw, LocationInfoInfoMessage.rw, InternalErrorMessage.rw, ErrorMessage.rw,
    WarningMessage.rw, InfoMessage.rw)
}

sealed trait UriTag extends Tag {
  def uri: Uri
}

object UriTag {
  implicit def rw: RW[UriTag] = RW.merge(FileLocationInfoErrorMessage.rw,
    FileLocationInfoWarningMessage.rw, FileLocationInfoInfoMessage.rw)
}


sealed trait LocationInfoTag extends Tag {
  def lineBegin: PosInteger

  def columnBegin: PosInteger

  def lineEnd: PosInteger

  def columnEnd: PosInteger

  def offset: Natural

  def length: Natural

  def toLocationError(fileUriOpt: Option[FileResourceUri],
                      kind: String, msg: String) =
    fileUriOpt match {
      case Some(fileUri) =>
        FileLocationInfoErrorMessage(
          kind = kind,
          uri = fileUri,
          lineBegin = lineBegin,
          columnBegin = columnBegin,
          lineEnd = lineEnd,
          columnEnd = columnEnd,
          offset = offset,
          length = length,
          message = msg)
      case _ =>
        LocationInfoErrorMessage(
          kind = kind,
          lineBegin = lineBegin,
          columnBegin = columnBegin,
          lineEnd = lineEnd,
          columnEnd = columnEnd,
          offset = offset,
          length = length,
          message = msg)
    }

  def toLocationWarning(fileUriOpt: Option[FileResourceUri],
                        kind: String, msg: String) =
    fileUriOpt match {
      case Some(fileUri) =>
        FileLocationInfoWarningMessage(
          kind = kind,
          uri = fileUri,
          lineBegin = lineBegin,
          columnBegin = columnBegin,
          lineEnd = lineEnd,
          columnEnd = columnEnd,
          offset = offset,
          length = length,
          message = msg)
      case _ =>
        LocationInfoWarningMessage(
          kind = kind,
          lineBegin = lineBegin,
          columnBegin = columnBegin,
          lineEnd = lineEnd,
          columnEnd = columnEnd,
          offset = offset,
          length = length,
          message = msg)
    }

  def toLocationInfo(fileUriOpt: Option[FileResourceUri],
                     kind: String, msg: String) =
    fileUriOpt match {
      case Some(fileUri) =>
        FileLocationInfoInfoMessage(
          kind = kind,
          uri = fileUri,
          lineBegin = lineBegin,
          columnBegin = columnBegin,
          lineEnd = lineEnd,
          columnEnd = columnEnd,
          offset = offset,
          length = length,
          message = msg)
      case _ =>
        LocationInfoInfoMessage(
          kind = kind,
          lineBegin = lineBegin,
          columnBegin = columnBegin,
          lineEnd = lineEnd,
          columnEnd = columnEnd,
          offset = offset,
          length = length,
          message = msg)
    }
}

object LocationInfoTag {
  implicit def rw: RW[LocationInfoTag] = RW.merge(FileLocationInfoErrorMessage.rw,
    FileLocationInfoWarningMessage.rw, FileLocationInfoInfoMessage.rw, LocationInfoErrorMessage.rw,
    LocationInfoWarningMessage.rw, LocationInfoInfoMessage.rw)
}

sealed trait SeverityTag extends Tag

object SeverityTag {
  implicit def rw: RW[SeverityTag] = RW.merge(InternalErrorTag.rw,
    ErrorTag.rw, WarningTag.rw, InfoTag.rw)
}

sealed trait InternalErrorTag extends SeverityTag

object InternalErrorTag {
  implicit def rw: RW[InternalErrorTag] = RW.merge()
}

sealed trait ErrorTag extends SeverityTag

object ErrorTag {
  implicit def rw: RW[ErrorTag] = RW.merge(FileLocationInfoErrorMessage.rw, LocationInfoErrorMessage.rw)
}

sealed trait WarningTag extends SeverityTag

object WarningTag {
  implicit def rw: RW[WarningTag] = RW.merge(FileLocationInfoWarningMessage.rw,
    LocationInfoWarningMessage.rw)
}

sealed trait InfoTag extends SeverityTag

object InfoTag {
  implicit def rw: RW[InfoTag] = RW.merge(FileLocationInfoInfoMessage.rw, LocationInfoInfoMessage.rw)
}

sealed trait MessageTag extends Tag {
  def message: String
}

object MessageTag {
  implicit def rw: RW[MessageTag] = RW.merge(FileLocationInfoErrorMessage.rw,
    FileLocationInfoWarningMessage.rw, FileLocationInfoInfoMessage.rw, LocationInfoErrorMessage.rw,
    LocationInfoWarningMessage.rw, LocationInfoInfoMessage.rw, InternalErrorMessage.rw, ErrorMessage.rw,
    WarningMessage.rw, InfoMessage.rw)
}


final case class
LocationInfo(lineBegin: PosInteger,
             columnBegin: PosInteger,
             lineEnd: PosInteger,
             columnEnd: PosInteger,
             offset: Natural,
             length: Natural)
  extends LocationInfoTag

object LocationInfo {
  implicit def rw: RW[LocationInfo] = macroRW
}


final case class
FileLocationInfoErrorMessage(kind: String,
                             uri: Uri,
                             lineBegin: PosInteger,
                             columnBegin: PosInteger,
                             lineEnd: PosInteger,
                             columnEnd: PosInteger,
                             offset: Natural,
                             length: Natural,
                             message: String)
  extends KindTag
  with UriTag
  with LocationInfoTag
  with ErrorTag
  with MessageTag

object FileLocationInfoErrorMessage {
  implicit def rw: RW[FileLocationInfoErrorMessage] = macroRW
}



final case class
FileLocationInfoWarningMessage(kind: String,
                               uri: Uri,
                               lineBegin: PosInteger,
                               columnBegin: PosInteger,
                               lineEnd: PosInteger,
                               columnEnd: PosInteger,
                               offset: Natural,
                               length: Natural,
                               message: String)
  extends KindTag
  with UriTag
  with LocationInfoTag
  with WarningTag
  with MessageTag

object FileLocationInfoWarningMessage {
  implicit def rw: RW[FileLocationInfoWarningMessage] = macroRW
}


final case class
FileLocationInfoInfoMessage(kind: String,
                            uri: Uri,
                            lineBegin: PosInteger,
                            columnBegin: PosInteger,
                            lineEnd: PosInteger,
                            columnEnd: PosInteger,
                            offset: Natural,
                            length: Natural,
                            message: String)
  extends KindTag
  with UriTag
  with LocationInfoTag
  with InfoTag
  with MessageTag

object FileLocationInfoInfoMessage {
  implicit def rw: RW[FileLocationInfoInfoMessage] = macroRW
}



final case class
LocationInfoErrorMessage(kind: String,
                         lineBegin: PosInteger,
                         columnBegin: PosInteger,
                         lineEnd: PosInteger,
                         columnEnd: PosInteger,
                         offset: Natural,
                         length: Natural,
                         message: String)
  extends KindTag
  with LocationInfoTag
  with ErrorTag
  with MessageTag

object LocationInfoErrorMessage {
  implicit def rw: RW[LocationInfoErrorMessage] = macroRW
}



final case class
LocationInfoWarningMessage(kind: String,
                           lineBegin: PosInteger,
                           columnBegin: PosInteger,
                           lineEnd: PosInteger,
                           columnEnd: PosInteger,
                           offset: Natural,
                           length: Natural,
                           message: String)
  extends KindTag
  with LocationInfoTag
  with WarningTag
  with MessageTag

object LocationInfoWarningMessage {
  implicit def rw: RW[LocationInfoWarningMessage] = macroRW
}


final case class
LocationInfoInfoMessage(kind: String,
                        lineBegin: PosInteger,
                        columnBegin: PosInteger,
                        lineEnd: PosInteger,
                        columnEnd: PosInteger,
                        offset: Natural,
                        length: Natural,
                        message: String)
  extends KindTag
  with LocationInfoTag
  with InfoTag
  with MessageTag

object LocationInfoInfoMessage {
  implicit def rw: RW[LocationInfoInfoMessage] = macroRW
}



final case class
InternalErrorMessage(kind: String,
                     message: String)
  extends InternalErrorTag
  with KindTag
  with MessageTag

object InternalErrorMessage {
  implicit def rw: RW[InternalErrorMessage] = macroRW
}



final case class
ErrorMessage(kind: String,
             message: String)
  extends ErrorTag
  with KindTag
  with MessageTag

object ErrorMessage {
  implicit def rw: RW[ErrorMessage] = macroRW
}


final case class
WarningMessage(kind: String,
               message: String)
  extends WarningTag
  with KindTag
  with MessageTag

object WarningMessage {
  implicit def rw: RW[WarningMessage] = macroRW
}



final case class
InfoMessage(kind: String,
            message: String)
  extends InfoTag
  with KindTag
  with MessageTag

object InfoMessage {
  implicit def rw: RW[InfoMessage] = macroRW
}
