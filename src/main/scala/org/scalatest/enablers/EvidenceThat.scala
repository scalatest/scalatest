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
    def containsOneOf(container: L, elements: scala.collection.Seq[R]): Boolean
  }
  def canBeContainedIn[L](constraint: ContainingConstraint[L, R]): CanBeContainedIn[L] =
    new CanBeContainedIn[L] {
      def contains(container: L, ele: R): Boolean = constraint.contains(container, ele)
      def containsOneOf(container: L, elements: scala.collection.Seq[R]): Boolean = constraint.containsOneOf(container, elements)
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

  implicit def specialConversion[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): EvidenceThat[org.scalatest.Entry[K, V]]#CanBeContainedIn[JMAP[K, V]] = {
    (new EvidenceThat[org.scalatest.Entry[K, V]]).canBeContainedIn[JMAP[K, V]] {
      // Here I need a ContainingConstraint[JMAP[K, V], org.scalatest.Entry[K, V]]
      new ContainingConstraint[JMAP[K, V], org.scalatest.Entry[K, V]] {

        import scala.collection.JavaConverters._

        def contains(map: JMAP[K, V], ele: org.scalatest.Entry[K, V]): Boolean = {
          map.entrySet.asScala.exists((e: java.util.Map.Entry[K, V]) => equality.areEqual(e, ele))
        }

        private val constraint: Constraint[java.util.Map.Entry[K, V], org.scalatest.Entry[K, V]] = new org.scalactic.TripleEqualsSupport.EqualityConstraint(equality)

        def containsOneOf(map: JMAP[K, V], elements: scala.collection.Seq[org.scalatest.Entry[K, V]]): Boolean = {
          val foundSet = ContainingConstraint.checkOneOf[java.util.Map.Entry[K, V], org.scalatest.Entry[K, V]](map.entrySet.asScala, elements, constraint)
          foundSet.size == 1
        }
        def containsNoneOf(map: JMAP[K, V], elements: scala.collection.Seq[org.scalatest.Entry[K, V]]): Boolean = {
          val found = ContainingConstraint.checkNoneOf[java.util.Map.Entry[K, V], org.scalatest.Entry[K, V]](map.entrySet.asScala, elements, constraint)
          !found.isDefined
        }
      }
    }
  }
}

