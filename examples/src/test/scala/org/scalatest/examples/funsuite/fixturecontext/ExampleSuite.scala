/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest.examples.funsuite.fixturecontext

import collection.mutable.ListBuffer
import org.scalatest.FunSuite

class ExampleSuite extends FunSuite {

  trait Builder {
    val builder = new StringBuilder("ScalaTest is ")
  }

  trait Buffer {
    val buffer = ListBuffer("ScalaTest", "is")
  }

  // This test needs the StringBuilder fixture
  test("Testing should be productive") {
    new Builder {
      builder.append("productive!")
      assert(builder.toString === "ScalaTest is productive!")
    }
  }

  // This test needs the ListBuffer[String] fixture
  test("Test code should be readable") {
    new Buffer {
      buffer += ("readable!")
      assert(buffer === List("ScalaTest", "is", "readable!"))
    }
  }

  // This test needs both the StringBuilder and ListBuffer
  test("Test code should be clear and concise") {
    new Builder with Buffer {
      builder.append("clear!")
      buffer += ("concise!")
      assert(builder.toString === "ScalaTest is clear!")
      assert(buffer === List("ScalaTest", "is", "concise!"))
    }
  }
}
