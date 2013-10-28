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
package org.scalatest.examples.featurespec.markup

import collection.mutable
import org.scalatest._

class SetSpec extends FeatureSpec with GivenWhenThen {

  markup { """

Mutable Set
-----------

A set is a collection that contains no duplicate elements.

To implement a concrete mutable set, you need to provide implementations
of the following methods:

    def contains(elem: A): Boolean
    def iterator: Iterator[A]
    def += (elem: A): this.type
    def -= (elem: A): this.type

If you wish that methods like `take`,
`drop`, `filter` return the same kind of set,
you should also override:

    def empty: This

It is also good idea to override methods `foreach` and
`size` for efficiency.

  """ }
  
  feature("An element can be added to an empty mutable Set") {
    scenario("When an element is added to an empty mutable Set") {
      Given("an empty mutable Set")
      val set = mutable.Set.empty[String]

      When("an element is added")
      set += "clarity"

      Then("the Set should have size 1")
      assert(set.size === 1)

      And("the Set should contain the added element")
      assert(set.contains("clarity"))

      markup("This test finished with a **bold** statement!")
    }
  }
}
