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
package org.scalatest.examples.featurespec.fixturecontext

import collection.mutable.ListBuffer
import org.scalatest.FeatureSpec

class ExampleSpec extends FeatureSpec {

  trait Builder {
    val builder = new StringBuilder("ScalaTest is designed to ")
  }

  trait Buffer {
    val buffer = ListBuffer("ScalaTest", "is", "designed", "to")
  }

  feature("Simplicity") {
    // This test needs the StringBuilder fixture
    scenario("User needs to read test code written by others") {
      new Builder {
        builder.append("encourage clear code!")
        assert(builder.toString === "ScalaTest is designed to encourage clear code!")
      }
    }
    
    // This test needs the ListBuffer[String] fixture
    scenario("User needs to understand what the tests are doing") {
      new Buffer {
        buffer += ("be", "easy", "to", "reason", "about!")
        assert(buffer === List("ScalaTest", "is", "designed", "to", "be", "easy", "to", "reason", "about!"))
      }
    }

    // This test needs both the StringBuilder and ListBuffer
    scenario("User needs to write tests") {
      new Builder with Buffer {
        builder.append("be easy to learn!")
        buffer += ("be", "easy", "to", "remember", "how", "to", "write!")
        assert(builder.toString === "ScalaTest is designed to be easy to learn!")
        assert(buffer === List("ScalaTest", "is", "designed", "to", "be", "easy",
          "to", "remember", "how", "to", "write!"))
      }
    }
  }
}
