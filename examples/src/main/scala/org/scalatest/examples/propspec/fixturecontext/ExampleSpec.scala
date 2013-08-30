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
package org.scalatest.examples.propspec.fixturecontext

import org.scalatest._
import org.scalatest.prop._
import scala.collection.immutable._

trait SetExamples extends Tables {
  def examples =
    Table(
      "set",
      bitSet,
      hashSet,
      treeSet
    )

  def bitSet: BitSet
  def hashSet: HashSet[Int]
  def treeSet: TreeSet[Int]
}

class EmptySetExamples extends SetExamples {
  def bitSet = BitSet.empty
  def hashSet = HashSet.empty[Int]
  def treeSet = TreeSet.empty[Int]
}

class SetSpec extends PropSpec with TableDrivenPropertyChecks with ShouldMatchers {

  property("an empty Set should have size 0") {
    new EmptySetExamples {
      forAll(examples) { set =>
        set.size should be (0)
      }
    }
  }

  property("invoking head on an empty set should produce NoSuchElementException") {
    new EmptySetExamples {
      forAll(examples) { set =>
        evaluating { set.head } should produce [NoSuchElementException]
      }
    }
  }
}



