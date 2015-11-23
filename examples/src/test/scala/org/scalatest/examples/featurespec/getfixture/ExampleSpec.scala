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
package org.scalatest.examples.featurespec.getfixture

import org.scalatest.FeatureSpec
import collection.mutable.ListBuffer

class ExampleSpec extends FeatureSpec {

  class Fixture {
    val builder = new StringBuilder("ScalaTest is designed to ")
    val buffer = new ListBuffer[String]
  }
  
  def fixture = new Fixture
  
  feature("Simplicity") {
    scenario("User needs to read test code written by others") {
      val f = fixture
      f.builder.append("encourage clear code!")
      assert(f.builder.toString === "ScalaTest is designed to encourage clear code!")
      assert(f.buffer.isEmpty)
      f.buffer += "sweet"
    }
  
    scenario("User needs to understand what the tests are doing") {
      val f = fixture
      f.builder.append("be easy to reason about!")
      assert(f.builder.toString === "ScalaTest is designed to be easy to reason about!")
      assert(f.buffer.isEmpty)
    }
  }
}
