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
package org.scalatest.enablers

import org.scalactic.Constraint
import org.scalactic.Equality

final class EvidenceThat[R] {
  abstract class CanEqual[L] {
    def areEqual(leftSide: L, rightSide: R): Boolean
  }
  def canEqual[L](implicit constraint: Constraint[L, R]): CanEqual[L] =
    new CanEqual[L] {
      def areEqual(leftSide: L, rightSide: R): Boolean = constraint.areEqual(leftSide, rightSide)
    }
  def canEqualByEquality[L](equality: Equality[L]): CanEqual[L] =
    new CanEqual[L] {
      def areEqual(leftSide: L, rightSide: R): Boolean = equality.areEqual(leftSide, rightSide)
    }
/*
  abstract class CanBeContainedIn[L] {
    def isContainedIn(leftSide: L, rightSide: R): Boolean
  }
  def canBeContainedIn[E, TRAV[e] <: scala.collection.GenTraversable[e]](constraint: Constraint[E, R]): CanBeContainedIn[TRAV[E]] =
    new CanBeContainedIn[TRAV[E]] {
      def isContainedIn(trav: TRAV[E], ele: R): Boolean = trav.exists((e: E) => constraint.areEqual(e, ele))
    }
*/
}
object EvidenceThat {
  implicit def constrainedEquality[L, R](implicit constraint: Constraint[L, R]): EvidenceThat[R]#CanEqual[L] =
    (new EvidenceThat[R]).canEqual[L]
/*
  implicit def constrainedHoldingForGenTraversable[E, TRAV[e] <: scala.collection.GenTraversable[e], R](implicit constraint: Constraint[E, R]): EvidenceThat[R]#CanBeContainedIn[TRAV[E]] =
    (new EvidenceThat[R]).canBeContainedIn[E, TRAV](constraint)
*/

  implicit def convertEqualityToEvidence[L, R](equality: Equality[L]): EvidenceThat[R]#CanEqual[L] =
    (new EvidenceThat[R]).canEqualByEquality[L](equality)
}

