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
import org.scalautils.Tolerance
import org.scalautils.Explicitly
import scala.annotation.tailrec
import org.scalautils.Equality
import org.scalatest.MatchersHelper.andMatchersAndApply
import org.scalatest.MatchersHelper.orMatchersAndApply
import org.scalatest.MatchersHelper.matchSymbolToPredicateMethod

private[scalatest] class JavaCollectionWrapper[T](underlying: java.util.Collection[T]) extends Traversable[T] {
  def foreach[U](f: (T) => U) {
    val javaIterator = underlying.iterator
    while (javaIterator.hasNext)
      f(javaIterator.next)
  }
  override def toString: String = if (underlying == null) "null" else underlying.toString
}

