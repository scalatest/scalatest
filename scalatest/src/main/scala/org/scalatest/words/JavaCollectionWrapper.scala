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

import scala.collection.Traversable
import scala.collection.JavaConverters._

private[scalatest] class JavaCollectionWrapper[T](underlying: java.util.Collection[T]) extends Traversable[T] {
  override def foreach[U](f: (T) => U): Unit = {
    val javaIterator = underlying.iterator
    while (javaIterator.hasNext)
      f(javaIterator.next)
  }

  def iterator: Iterator[T] = underlying.iterator().asScala

  override def toString: String = if (underlying == null) "null" else underlying.toString
}
