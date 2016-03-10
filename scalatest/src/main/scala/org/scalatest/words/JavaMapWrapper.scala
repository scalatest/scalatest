/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.words

import org.scalatest.matchers._
import org.scalatest.enablers._
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.util.matching.Regex
import java.lang.reflect.Field
import scala.reflect.Manifest
import org.scalatest.MatchersHelper.transformOperatorChars
import scala.collection.Traversable
import org.scalatest.Assertions.areEqualComparingArraysStructurally
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import org.scalactic.Tolerance
import org.scalactic.Explicitly
import scala.annotation.tailrec
import org.scalactic.Equality
import org.scalatest.MatchersHelper.andMatchersAndApply
import org.scalatest.MatchersHelper.orMatchersAndApply

private[scalatest] class JavaMapWrapper[K, V](val underlying: java.util.Map[K, V]) extends scala.collection.Map[K, V] {
  // Even though the java map is mutable I just wrap it it to a plain old Scala map, because
  // I have no intention of mutating it.
  override def size: Int = underlying.size
  def get(key: K): Option[V] =
    if (underlying.containsKey(key)) Some(underlying.get(key)) else None
  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    private val javaIterator = underlying.entrySet.iterator
    def next: (K, V) = {
      val nextEntry = javaIterator.next
      (nextEntry.getKey, nextEntry.getValue)
    }
    def hasNext: Boolean = javaIterator.hasNext
  }
  override def +[W >: V] (kv: (K, W)): scala.collection.Map[K, W] = {
    val newJavaMap = new java.util.LinkedHashMap[K, W](underlying)
    val (key, value) = kv
    newJavaMap.put(key, value)
    new JavaMapWrapper[K, W](newJavaMap)
  }
  override def - (key: K): scala.collection.Map[K, V] = {
    val newJavaMap = new java.util.LinkedHashMap[K, V](underlying)
    newJavaMap.remove(key)
    new JavaMapWrapper[K, V](underlying)
  }
  override def empty = new JavaMapWrapper[K, V](new java.util.LinkedHashMap[K, V]())
  override def toString: String = if (underlying == null) "null" else underlying.toString
}

