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
package org.scalatest.examples.refspec.info

import org.scalatest._
import refspec.RefSpec
import collection.mutable
 
class SetSpec extends RefSpec with GivenWhenThen {
  
  object `A mutable Set` {
    def `should allow an element to be added` {
      Given("an empty mutable Set")
      val set = mutable.Set.empty[String]

      When("an element is added")
      set += "clarity"

      Then("the Set should have size 1")
      assert(set.size === 1)

      And("the Set should contain the added element")
      assert(set.contains("clarity"))

      info("That's all folks!")
    }
  }
}
