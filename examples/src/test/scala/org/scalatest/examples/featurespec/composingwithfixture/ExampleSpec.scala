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
package org.scalatest.examples.featurespec.composingwithfixture

import org.scalatest._
import collection.mutable.ListBuffer

trait Builder extends TestSuiteMixin { this: TestSuite =>

  val builder = new StringBuilder

  abstract override def withFixture(test: NoArgTest) = {
    builder.append("ScalaTest is designed to ")
    try super.withFixture(test) // To be stackable, must call super.withFixture
    finally builder.clear()
  }
}

trait Buffer extends TestSuiteMixin { this: TestSuite =>

  val buffer = new ListBuffer[String]

  abstract override def withFixture(test: NoArgTest) = {
    try super.withFixture(test) // To be stackable, must call super.withFixture
    finally buffer.clear()
  }
}

class ExampleSpec extends FeatureSpec with Builder with Buffer {

  feature("Simplicity") {
    scenario("User needs to read test code written by others") {
      builder.append("encourage clear code!")
      assert(builder.toString === "ScalaTest is designed to encourage clear code!")
      assert(buffer.isEmpty)
      buffer += "clear"
    }

    scenario("User needs to understand what the tests are doing") {
      builder.append("be easy to reason about!")
      assert(builder.toString === "ScalaTest is designed to be easy to reason about!")
      assert(buffer.isEmpty)
      buffer += "easy"
    }
  }
}
