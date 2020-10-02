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

import scala.collection.mutable


object MIdMap {
  def apply[K <: AnyRef, V](): MIdMap[K, V] = new MIdMap[K, V]

  def apply[K <: AnyRef, V](kvs: (K, V)*): MIdMap[K, V] = {
    val r = new MIdMap[K, V]
    for ((k, v) <- kvs) {
      r.put(k, v)
    }
    r
  }
}



final class MIdMap[K <: AnyRef, V] extends java.util.IdentityHashMap[K, V]() {
  //
//  override def elemEquals(key1: K, key2: K): Boolean =
//    key1 eq key2
//
//  override def elemHashCode(key: K): Int =
//    System.identityHashCode(key)

  override def clone: MIdMap[K, V] = {
    import scala.jdk.CollectionConverters._
    val c : MIdMap[K, V] = MIdMap[K, V](this.asScala.toSeq: _*)
    c.putAll(this)
    c
  }

  def toMap() : mutable.Map[K, V] = {
    import scala.jdk.CollectionConverters._
    this.asScala
  }

  def apply(k : K) : V = {
    this.get(k)
  }

  def getOrElseUpdate(k : K, v : V) : V = {
    if(this.containsKey(k)) {
      this.get(k)
    } else {
      this.put(k,v)
      this.get(k)
    }
  }


}
