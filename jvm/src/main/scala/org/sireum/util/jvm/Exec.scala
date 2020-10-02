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

package org.sireum.util.jvm

import java.io._
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import org.sireum.util._

import scala.sys.process.ProcessIO

object Exec {

  sealed abstract class Result

  object Timeout extends Result

  case class ExceptionRaised(e: Exception) extends Result

  case class StringResult(s: String, exitValue: Int) extends Result

  {
    System.setProperty("jna.nosys", "true")
  }
}

final class Exec {

  val env: MMap[String, String] = mmapEmpty

  def process(args: Seq[String],
              writeInput: OutputStream => Unit,
              processOutput: InputStream => Unit,
              extraEnv: (String, String)*): scala.sys.process.Process = {
    scala.sys.process.Process({
      val pb = new java.lang.ProcessBuilder(args: _*)
      pb.redirectErrorStream(true)
      val m = pb.environment
      for ((k, v) <- env ++ extraEnv) {
        m.put(k, v)
      }
      pb
    }).run(new ProcessIO(writeInput, processOutput, errorF).daemonized())
  }

  def run(waitTime: Long, args: Seq[String], input: Option[String],
          extraEnv: (String, String)*): Exec.Result =
    run(waitTime, args, input, None, extraEnv: _*)

  def run(waitTime: Long, args: Seq[String], input: Option[String],
          dir: Option[File], extraEnv: (String, String)*): Exec.Result = {
    import scala.jdk.CollectionConverters._
    val m = mmapEmpty[String, String]
    for ((k, v) <- System.getenv().asScala ++ env ++ extraEnv) {
      m.put(k, v)
    }
    val out = new java.io.ByteArrayOutputStream()
    def f(baos: java.io.ByteArrayOutputStream)(bytes: Array[Byte], n: Int): Unit = {
      baos.write(bytes, 0, n)
    }
    val pOut = _root_.os.ProcessOutput(f(out))
    val stdin: _root_.os.ProcessInput = input match {
      case Some(s) => s
      case _ => _root_.os.Pipe
    }
    val sp = _root_.os.proc(args).
      spawn(cwd = dir.map(d => _root_.os.Path(d.getCanonicalPath)).getOrElse(os.pwd),
        env = m.toMap, stdin = stdin, stdout = pOut, stderr = pOut,
        mergeErrIntoOut = true, propagateEnv = false)
    val term = sp.waitFor(if (waitTime > 0) waitTime else -1)
    if (term)
      return Exec.StringResult(out.toString(StandardCharsets.UTF_8.name), sp.exitCode())
    if (sp.isAlive()) {
      try {
        sp.destroy()
        sp.wrapped.waitFor(500, TimeUnit.MICROSECONDS)
      } catch {
        case _: Throwable =>
      }
      if (sp.isAlive())
        try sp.destroyForcibly()
        catch {
          case _: Throwable =>
        }
    }
    Exec.Timeout
  }

  def errorF(is: InputStream) = {
    try while (is.read != -1) {} finally is.close()
  }
}