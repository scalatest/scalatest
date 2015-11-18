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
package org.scalatest.examples.freespec.getfixture

import org.scalatest.FreeSpec
import collection.mutable.ListBuffer

class ExampleSpec extends FreeSpec {

  class Fixture {
    val builder = new StringBuilder("ScalaTest is ")
    val buffer = new ListBuffer[String]
  }
  
  def fixture = new Fixture
  
  "Testing" - {
    "should be easy" in {
      val f = fixture
      f.builder.append("easy!")
      assert(f.builder.toString === "ScalaTest is easy!")
      assert(f.buffer.isEmpty)
      f.buffer += "sweet"
    }
  
    "should be fun" in {
      val f = fixture
      f.builder.append("fun!")
      assert(f.builder.toString === "ScalaTest is fun!")
      assert(f.buffer.isEmpty)
    }
  }
}
