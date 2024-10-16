/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.verbs

import org.scalatest.FailureMessages
import org.scalactic.Prettifier

/**
 * This wrapper gives better toString (Array(x, x, x)) as compared to Scala default one (WrappedArray(x, x, x)).
 */
private[scalatest] class ArrayWrapper[T](underlying: Array[T]) extends Traversable[T] {
  override def foreach[U](f: (T) => U): Unit = {
    var index = 0
    while (index < underlying.length) {
      index += 1
      f(underlying(index - 1))
    }
  }

  def iterator: Iterator[T] = underlying.iterator

  // Need to prettify the array's toString, because by the time it gets to decorateToStringValue, the array
  // has been wrapped in this Traversable and so it won't get prettified anymore by FailureMessages.decorateToStringValue.
  override def toString: String = FailureMessages.decorateToStringValue(Prettifier.default, underlying)
}

