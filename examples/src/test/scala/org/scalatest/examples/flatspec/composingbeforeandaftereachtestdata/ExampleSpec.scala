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
package org.scalatest.examples.flatspec.composingbeforeandaftereachtestdata

import org.scalatest._
import collection.mutable.ListBuffer

trait Builder extends BeforeAndAfterEachTestData { this: Suite =>

  val builder = new StringBuilder

  override def beforeEach(td: TestData) {
    builder.append(td.name)
    super.beforeEach(td) // To be stackable, must call super.beforeEach(TestData)
  }

  override def afterEach(td: TestData) {
    try {
      super.afterEach(td) // To be stackable, must call super.afterEach(TestData)
    }
    finally {
      builder.clear()
    }
  }
}

trait Buffer extends BeforeAndAfterEachTestData { this: Suite =>

  val buffer = new ListBuffer[String]

  override def afterEach(td: TestData) {
    try {
      super.afterEach(td) // To be stackable, must call super.afterEach(TestData)
    }
    finally {
      buffer.clear()
    }
  }
}

class ExampleSpec extends FlatSpec with Builder with Buffer {

  "Testing" should "be easy" in {
    builder.append("!")
    assert(builder.toString === "Testing should be easy!")
    assert(buffer.isEmpty)
    buffer += "sweet"
  }

  it should "be fun" in {
    builder.append("!")
    assert(builder.toString === "Testing should be fun!")
    assert(buffer.isEmpty)
    buffer += "clear"
  }
}
