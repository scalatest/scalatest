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

import org.scalactic.{Equality, One, Many}

import scala.language.{higherKinds, implicitConversions}

/**
 * An [[Equality]] that allows the comparison of values nested in [[One]]s using whatever Equality is
 * in scope for the contained type.
 */
trait RecursiveOneEquality {
  implicit def recursiveOneEquality[E, ONE[e] <: One[e]](implicit eqE: Equality[E]): Equality[ONE[E]] =
    new Equality[ONE[E]] {
      def areEqual(oneA: ONE[E], other: Any): Boolean = (oneA, other) match {
        case (One(a), One(b)) => eqE.areEqual(a, b)
        case _ => false
      }
    }
}

/**
 * An [[Equality]] that allows the comparison of values nested in [[Many]]s using whatever Equality is
 * in scope for the contained type.
 */
trait RecursiveManyEquality {
  implicit def recursiveManyEquality[E, MANY[e] <: Many[e]](implicit eqE: Equality[E]): Equality[MANY[E]] =
    new Equality[MANY[E]] {
      def areEqual(manyA: MANY[E], b: Any): Boolean = b match {
        case manyB: Many[_] => manyA.corresponds(manyB) { case (eleA, eleB) => eqE.areEqual(eleA, eleB) }
        case _ => false
      }
    }
}

object RecursiveEveryEquality extends RecursiveOneEquality with RecursiveManyEquality
