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
package org.scalactic

private[scalactic] trait EquiSets[T] { thisEquiSets =>

  val equivalence: Equivalence[T]

  class EquiSet[T] private (underlying: Vector[T]) {
    def isEmpty: Boolean = underlying.isEmpty
    def size: Int = underlying.size
    // def union[E](that: thisEquiSets.EquiSet[T]) = 1
  }
  object EquiSet {
    def empty: EquiSet[T] = new EquiSet(Vector.empty)
    def apply(elems: T*): EquiSet[T] = 
      new EquiSet(Vector(elems: _*))
  }
}

