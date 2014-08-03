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
package org.scalactic.enablers

import org.scalactic.EqualityConstraint
import org.scalactic.Equality
import scala.collection.GenTraversable
import scala.language.implicitConversions
import scala.language.higherKinds

final class EvidenceThat[R] {

  abstract class CanEqual[L] {
    def areEqual(leftSide: L, rightSide: R): Boolean
  }

  def canEqualByConstraint[L](implicit constraint: EqualityConstraint[L, R]): CanEqual[L] =
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
    def containsNoneOf(container: L, elements: scala.collection.Seq[R]): Boolean
  }

  def canBeContainedIn[L](constraint: ContainingConstraint[L, R]): CanBeContainedIn[L] =
    new CanBeContainedIn[L] {
      def contains(container: L, ele: R): Boolean = constraint.contains(container, ele)
      def containsOneOf(container: L, elements: scala.collection.Seq[R]): Boolean = constraint.containsOneOf(container, elements)
      def containsNoneOf(container: L, elements: scala.collection.Seq[R]): Boolean = constraint.containsNoneOf(container, elements)
    }

  abstract class CanBeContainedInAggregation[L] {
    def containsAtLeastOneOf(aggregation: L, eles: Seq[R]): Boolean
    def containsTheSameElementsAs(leftAggregation: L, rightAggregation: GenTraversable[R]): Boolean
    def containsOnly(aggregation: L, eles: Seq[R]): Boolean
    def containsAllOf(aggregation: L, eles: Seq[R]): Boolean
    def containsAtMostOneOf(aggregation: L, eles: Seq[R]): Boolean
  }

  def canBeContainedInAggregation[L](constraint: AggregatingConstraint[L, R]): CanBeContainedInAggregation[L] =
    new CanBeContainedInAggregation[L] {
      def containsAtLeastOneOf(aggregation: L, eles: Seq[R]): Boolean = constraint.containsAtLeastOneOf(aggregation, eles)
      def containsTheSameElementsAs(leftAggregation: L, rightAggregation: GenTraversable[R]): Boolean = constraint.containsTheSameElementsAs(leftAggregation, rightAggregation)
      def containsOnly(aggregation: L, eles: Seq[R]): Boolean = constraint.containsOnly(aggregation, eles)
      def containsAllOf(aggregation: L, eles: Seq[R]): Boolean = constraint.containsAllOf(aggregation, eles)
      def containsAtMostOneOf(aggregation: L, eles: Seq[R]): Boolean = constraint.containsAtMostOneOf(aggregation, eles)
    }

  abstract class CanBeContainedInSequence[L] {
    def containsInOrder(sequence: L, eles: Seq[R]): Boolean
    def containsInOrderOnly(sequence: L, eles: Seq[R]): Boolean
    def containsTheSameElementsInOrderAs(leftSequence: L, rightSequence: GenTraversable[R]): Boolean
  }

  def canBeContainedInSequence[L](constraint: SequencingConstraint[L, R]): CanBeContainedInSequence[L] =
    new CanBeContainedInSequence[L] {
      def containsInOrder(sequence: L, eles: Seq[R]): Boolean = constraint.containsInOrder(sequence, eles)
      def containsInOrderOnly(sequence: L, eles: Seq[R]): Boolean = constraint.containsInOrderOnly(sequence, eles)
      def containsTheSameElementsInOrderAs(leftSequence: L, rightSequence: GenTraversable[R]): Boolean = constraint.containsTheSameElementsInOrderAs(leftSequence, rightSequence)
    }
}
object EvidenceThat {
  implicit def constrainedEquality[L, R](implicit constraint: EqualityConstraint[L, R]): EvidenceThat[R]#CanEqual[L] =
    (new EvidenceThat[R]).canEqualByConstraint[L]
  implicit def convertEqualityToEvidenceThatRCanEqualL[L, R](equality: Equality[L])(implicit constraint: EqualityConstraint[L, R]): EvidenceThat[R]#CanEqual[L] =
    (new EvidenceThat[R]).canEqualByEquality[L](equality)

  implicit def constrainedContaining[L, R](implicit constraint: ContainingConstraint[L, R]): EvidenceThat[R]#CanBeContainedIn[L] =
    (new EvidenceThat[R]).canBeContainedIn[L](constraint)

  implicit def convertEqualityToEvidenceThatRCanBeContainedInL[L, R](equality: Equality[R])(implicit cvt: Equality[R] => ContainingConstraint[L, R]): EvidenceThat[R]#CanBeContainedIn[L] =
    (new EvidenceThat[R]).canBeContainedIn[L](cvt(equality))

  implicit def enableScalaTestEntryContaining[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): EvidenceThat[org.scalatest.Entry[K, V]]#CanBeContainedIn[JMAP[K, V]] = {
    (new EvidenceThat[org.scalatest.Entry[K, V]]).canBeContainedIn[JMAP[K, V]] {
      // Here I need a ContainingConstraint[JMAP[K, V], org.scalatest.Entry[K, V]]
      new ContainingConstraint[JMAP[K, V], org.scalatest.Entry[K, V]] {

        import scala.collection.JavaConverters._

        def contains(map: JMAP[K, V], ele: org.scalatest.Entry[K, V]): Boolean = {
          map.entrySet.asScala.exists((e: java.util.Map.Entry[K, V]) => equality.areEqual(e, ele))
        }

        private val constraint: EqualityConstraint[java.util.Map.Entry[K, V], org.scalatest.Entry[K, V]] = new org.scalactic.TripleEqualsSupport.BasicEqualityConstraint(equality)

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

  implicit def constrainedAggregating[L, R](implicit constraint: AggregatingConstraint[L, R]): EvidenceThat[R]#CanBeContainedInAggregation[L] =
    (new EvidenceThat[R]).canBeContainedInAggregation[L](constraint)

  implicit def convertEqualityToEvidenceThatRCanBeContainedInAggregationL[L, R](equality: Equality[R])(implicit cvt: Equality[R] => AggregatingConstraint[L, R]): EvidenceThat[R]#CanBeContainedInAggregation[L] =
    (new EvidenceThat[R]).canBeContainedInAggregation[L](cvt(equality))

  implicit def enableScalaTestEntryAggregating[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): EvidenceThat[org.scalatest.Entry[K, V]]#CanBeContainedInAggregation[JMAP[K, V]] = {
    (new EvidenceThat[org.scalatest.Entry[K, V]]).canBeContainedInAggregation[JMAP[K, V]] {
      // Here I need an AggregatingConstraint[JMAP[K, V], org.scalatest.Entry[K, V]]
      new AggregatingConstraint[JMAP[K, V], org.scalatest.Entry[K, V]] {

        import scala.collection.JavaConverters._

        private val constraint: EqualityConstraint[java.util.Map.Entry[K, V], org.scalatest.Entry[K, V]] = new org.scalactic.TripleEqualsSupport.BasicEqualityConstraint(equality)

        def containsAtLeastOneOf(map: JMAP[K, V], elements: scala.collection.Seq[org.scalatest.Entry[K, V]]): Boolean = {
          map.entrySet.asScala.exists((e: java.util.Map.Entry[K, V]) => elements.exists((ele: org.scalatest.Entry[K, V]) => constraint.areEqual(e, ele)))
        }
        def containsTheSameElementsAs(map: JMAP[K, V], elements: GenTraversable[org.scalatest.Entry[K, V]]): Boolean = {
          AggregatingConstraint.checkTheSameElementsAs(map.entrySet.asScala, elements, constraint)
        }
        def containsOnly(map: JMAP[K, V], elements: scala.collection.Seq[org.scalatest.Entry[K, V]]): Boolean = {
          AggregatingConstraint.checkOnly(map.entrySet.asScala, elements, constraint)
        }
        def containsAllOf(map: JMAP[K, V], elements: scala.collection.Seq[org.scalatest.Entry[K, V]]): Boolean = {
          AggregatingConstraint.checkAllOf(map.entrySet.asScala, elements, constraint)
        }
        def containsAtMostOneOf(map: JMAP[K, V], elements: scala.collection.Seq[org.scalatest.Entry[K, V]]): Boolean = {
          AggregatingConstraint.checkAtMostOneOf(map.entrySet.asScala, elements, constraint)
        }
      }
    }
  }
  implicit def constrainedSequencing[L, R](implicit constraint: SequencingConstraint[L, R]): EvidenceThat[R]#CanBeContainedInSequence[L] =
    (new EvidenceThat[R]).canBeContainedInSequence[L](constraint)

  implicit def convertEqualityToEvidenceThatRCanBeContainedInSequenceL[L, R](equality: Equality[R])(implicit cvt: Equality[R] => SequencingConstraint[L, R]): EvidenceThat[R]#CanBeContainedInSequence[L] =
    (new EvidenceThat[R]).canBeContainedInSequence[L](cvt(equality))

  implicit def enableScalaTestEntrySequencing[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): EvidenceThat[org.scalatest.Entry[K, V]]#CanBeContainedInSequence[JMAP[K, V]] = {
    (new EvidenceThat[org.scalatest.Entry[K, V]]).canBeContainedInSequence[JMAP[K, V]] {
      // Here I need an SequencingConstraint[JMAP[K, V], org.scalatest.Entry[K, V]]
      new SequencingConstraint[JMAP[K, V], org.scalatest.Entry[K, V]] {

        import scala.collection.JavaConverters._

        private val constraint: EqualityConstraint[java.util.Map.Entry[K, V], org.scalatest.Entry[K, V]] = new org.scalactic.TripleEqualsSupport.BasicEqualityConstraint(equality)

        def containsInOrder(map: JMAP[K, V], elements: scala.collection.Seq[org.scalatest.Entry[K, V]]): Boolean = {
          SequencingConstraint.checkInOrder(map.entrySet.iterator.asScala.toVector, elements, constraint)
        }

        def containsInOrderOnly(map: JMAP[K, V], elements: scala.collection.Seq[org.scalatest.Entry[K, V]]): Boolean = {
          SequencingConstraint.checkInOrderOnly(map.entrySet.iterator.asScala.toVector, elements, constraint)
        }

        def containsTheSameElementsInOrderAs(map: JMAP[K, V], elements: GenTraversable[org.scalatest.Entry[K, V]]): Boolean = {
          SequencingConstraint.checkTheSameElementsInOrderAs(map.entrySet.iterator.asScala.toVector, elements, constraint)
        }
      }
    }
  }
}

