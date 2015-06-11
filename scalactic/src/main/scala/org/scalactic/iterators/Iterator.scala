/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalactic.iterators

trait Iterator[+A] extends Any with GenIterableOnce[A] {
  def hasNext: Boolean
  def next(): A
}

private[scalactic] object Iterator {
  def apply[A](it: scala.collection.Iterator[A]): org.scalactic.iterators.Iterator[A] = 
      new Iterator[A] {
        def hasNext: Boolean = it.hasNext
        def next(): A = it.next()
        def iterator: Iterator[A] = this
        def toStandardList: scala.collection.immutable.List[A] = it.toList
      }
}
