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
package org.scalatest.examples.fixture.propspec.multi

import org.scalatest._
import prop.PropertyChecks
import scala.collection.mutable.ListBuffer

class ExampleSpec extends fixture.PropSpec with PropertyChecks with ShouldMatchers {

  case class FixtureParam(builder: StringBuilder, buffer: ListBuffer[String])

  def withFixture(test: OneArgTest) = {

    // Create needed mutable objects
    val stringBuilder = new StringBuilder("ScalaTest is ")
    val listBuffer = new ListBuffer[String]
    val theFixture = FixtureParam(stringBuilder, listBuffer)

    // Invoke the test function, passing in the mutable objects
    withFixture(test.toNoArgTest(theFixture))
  }

  property("testing should be easy") { f =>
    f.builder.append("easy!")
    assert(f.builder.toString === "ScalaTest is easy!")
    assert(f.buffer.isEmpty)
    val firstChar = f.builder(0)
    forAll { (c: Char) =>
      whenever (c != 'S') {
        c should not equal firstChar
      }
    }
    f.buffer += "sweet"
  }

  property("testing should be fun") { f =>
    f.builder.append("fun!")
    assert(f.builder.toString === "ScalaTest is fun!")
    assert(f.buffer.isEmpty)
    val firstChar = f.builder(0)
    forAll { (c: Char) =>
      whenever (c != 'S') {
        c should not equal firstChar
      }
    }
  }
}
