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

import java.text._
import org.scalatest._
import scala.collection.GenTraversable
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable.ParArray

class SortedCollectionsSpec extends UnitSpec {
  "The SortedCollections object" should "offer an apply method that takes an implicit OrderingEquality that picks up an implicit Ordering" in {
    val sortedIntColls = SortedCollections[Int]
    import sortedIntColls.immutable._
    SortedSet(9, 8, 2, 1, 2, 3).iterator.toList shouldEqual (List(1, 2, 3, 8, 9))
  }
  it should "treat arrays structurally (given an Ordering for arrays, because no implicit Ordering for arrays is provided by Scala)" in { 
    // This is contrived, but just to make sure arrays are treated structurally, I make up an Ordering for Array[Int] that
    // just adds up the ints and uses the sum to define the order.
    implicit val arrayOrdering =
      new Ordering[Array[Int]] {
        def compare(a: Array[Int], b: Array[Int]): Int = a.sum - b.sum
      }
    val intArrayColls = SortedCollections[Array[Int]]
    import intArrayColls.immutable._
    SortedSet(Array(9, 8, 7), Array(1, 2, 3), Array(4, 5, 6)).iterator.toList.map(_.toList) shouldEqual  List(List(1, 2, 3), List(4, 5, 6), List(9, 8, 7))
  }
}

