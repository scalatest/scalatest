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

import org.scalactic.{Equality, Good, Bad}

import scala.language.{higherKinds, implicitConversions}

/**
 * An [[Equality]] that allows the comparison of values nested in [[Or]]s using whatever Equality is
 * in scope for the contained [[Good]] and/or [[Bad]] type.
 */
trait RecursiveOrEquality {
  implicit def recursiveGoodEquality[G, GOOD[g] <: Good[g]](implicit eqG: Equality[G]): Equality[Good[G]] =
    new Equality[Good[G]] {
      override def areEqual(good: Good[G], other: Any): Boolean = (good, other) match {
        case (Good(g), Good(otherG)) => eqG.areEqual(g, otherG)
        case _ => false
      }
    }

  implicit def recursiveBadEquality[B, BAD[b] <: Bad[b]](implicit eqB: Equality[B]): Equality[Bad[B]] =
    new Equality[Bad[B]] {
      override def areEqual(good: Bad[B], other: Any): Boolean = (good, other) match {
        case (Bad(g), Bad(otherB)) => eqB.areEqual(g, otherB)
        case _ => false
      }
    }
}

object RecursiveOrEquality extends RecursiveOrEquality
