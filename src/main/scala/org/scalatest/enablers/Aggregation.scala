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

import org.scalautils.Equality

trait Aggregation[A] {
  def containsAtLeastOneOf(aggregation: A, eles: Seq[Any]): Boolean
/*
  def containsTheSameElementsAs(aggregation: A, it: Iterator[Any]): Boolean
  def containsTheSameElementsInOrderAs(aggregation: A, it: Iterator[Any]): Boolean
  def containsAllOf(aggregation: A, eles: Seq[Any]): Boolean
  def containsAtMostOneOf(aggregation: A, eles: Seq[Any]): Boolean
  def containsOnly(aggregation: A, eles: Seq[Any]): Boolean
  def containsInOrderOnly(aggregation: A, eles: Seq[Any]): Boolean
*/
}

object Aggregation {

  implicit def withGenTraversableElementEquality[E, TRAV[_] <: scala.collection.GenTraversable[_]](implicit equality: Equality[E]): Aggregation[TRAV[E]] = 
    new Aggregation[TRAV[E]] {
      def containsAtLeastOneOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        trav.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToGenTraversableAggregation[E, TRAV[_] <: scala.collection.GenTraversable[_]](equality: Equality[E]): Aggregation[TRAV[E]] = 
    withGenTraversableElementEquality(equality)
}
