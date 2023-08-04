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
import java.nio.ByteBuffer
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

    val chunks = new java.util.concurrent.ConcurrentLinkedQueue[Either[geny.Bytes, geny.Bytes]]
    val p = os.proc(args)
    val cwd = dir.map(d => os.Path(d.getCanonicalPath)).orNull
    val env = if (extraEnv.isEmpty) null else Map[String, String]() ++ extraEnv
    val stdin = input.map(in => os.ProcessInput.makeSourceInput(in)).getOrElse(os.Pipe)
    val timeout = if (waitTime > 0) waitTime else Long.MaxValue
    val sub = p.spawn(
      cwd, env,
      stdin,
      os.ProcessOutput.ReadBytes(
        (buf, n) => chunks.add(Left(new geny.Bytes(java.util.Arrays.copyOf(buf, n))))
      ),
      os.ProcessOutput.ReadBytes(
        (buf, n) => chunks.add(Right(new geny.Bytes(java.util.Arrays.copyOf(buf, n))))
      ),
      mergeErrIntoOut = true,
      propagateEnv = true
    )
    import scala.jdk.CollectionConverters._

    if (!sub.join(timeout)) {
      return Exec.Timeout
    }

    val chunksArr = chunks.iterator.asScala.toSeq
    val r = os.CommandResult(sub.exitCode(), chunksArr)
    if (r.exitCode == 0) {
      Exec.StringResult(r.out.text(), 0)
    } else {
      Exec.ExceptionRaised(new RuntimeException(r.out.text()))
    }
 }

  def errorF(is: InputStream): Unit = {
    try while (is.read != -1) {} finally is.close()
  }
}