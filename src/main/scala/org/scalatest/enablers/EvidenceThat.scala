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
  def canEqualByConstraint[L](implicit constraint: Constraint[L, R]): CanEqual[L] =
    new CanEqual[L] {
      def areEqual(leftSide: L, rightSide: R): Boolean = constraint.areEqual(leftSide, rightSide)
    }
  def canEqualByEquality[L](equality: Equality[L]): CanEqual[L] =
    new CanEqual[L] {
      def areEqual(leftSide: L, rightSide: R): Boolean = equality.areEqual(leftSide, rightSide)
    }
  abstract class CanBeContainedIn[L] {
    def contains(leftSide: L, rightSide: R): Boolean
  }
  def canBeContainedIn[L](constraint: ContainingConstraint[L, R]): CanBeContainedIn[L] =
    new CanBeContainedIn[L] {
      def contains(container: L, ele: R): Boolean = constraint.contains(container, ele)
    }
}
object EvidenceThat {
  implicit def constrainedEquality[L, R](implicit constraint: Constraint[L, R]): EvidenceThat[R]#CanEqual[L] =
    (new EvidenceThat[R]).canEqualByConstraint[L]
  implicit def convertEqualityToEvidenceThatRCanEqualL[L, R](equality: Equality[L])(implicit constraint: Constraint[L, R]): EvidenceThat[R]#CanEqual[L] =
    (new EvidenceThat[R]).canEqualByEquality[L](equality)

  implicit def constrainedContaining[L, R](implicit constraint: ContainingConstraint[L, R]): EvidenceThat[R]#CanBeContainedIn[L] =
    (new EvidenceThat[R]).canBeContainedIn[L](constraint)

  implicit def convertEqualityToEvidenceThatRCanBeContainedInL[L, R](equality: Equality[R])(implicit cvt: Equality[R] => ContainingConstraint[L, R]): EvidenceThat[R]#CanBeContainedIn[L] =
    (new EvidenceThat[R]).canBeContainedIn[L](cvt(equality))

/*
  implicit def convertEqualityToEvidenceThatRCanBeContainedInL[L, R](equality: Equality[R])(implicit constraint: ContainingConstraint[L, R]): EvidenceThat[R]#CanBeContainedIn[L] =
    (new EvidenceThat[R]).canBeContainedInByEquality[L](constraint)

  implicit def convertEqualityToEveryContainingConstraint[E, R](equality: Equality[E])(constraint: ContainingConstraint[Every[E], R]): ContainingConstraint[Every[E], R] =
    containingNatureOfEvery[E, R](equality)
*/
}

