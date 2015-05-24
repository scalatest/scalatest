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
package org.scalactic.equalities

import org.scalactic.Equality

import scala.language.{higherKinds, implicitConversions}

/**
 * An [[Equality]] that allows the comparison of values nested in Arrays using whatever Equality is
 * in scope for the contained types.
 */
trait RecursiveArrayEquality {
  implicit def recursiveArrayEquality[E, ARRAY[e] <: Array[e]](implicit eqE: Equality[E]): Equality[ARRAY[E]] =
    new Equality[ARRAY[E]] {
      def areEqual(arrA: ARRAY[E], other: Any): Boolean = other match {
        case arrB: Array[_] => arrA.corresponds(arrB) {
          case (eleA, eleB) => eqE.areEqual(eleA, eleB)
        }
        case _ => false
      }
    }
}

object RecursiveArrayEquality extends RecursiveArrayEquality
