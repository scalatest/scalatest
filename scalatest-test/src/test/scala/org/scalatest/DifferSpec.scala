/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalatest

import org.scalactic._
import exceptions.TestFailedException
import SharedHelpers.thisLineNumber

class DifferSpec extends FunSpec {

  import Matchers._

  describe("Differ") {

    implicit val equality =
      new Equality[String] with Differ[String] {
        def areEqual(a: String, b: Any): Boolean = Equality.default.areEqual(a, b)
        def difference(a: String, b: Any): Difference =
          new Difference {
            def inlineDiff = {
              val (aa, bb) = Suite.getObjectsForFailureMessage(a, b)
              Some(("**" + aa, bb + "**"))
            }
            def sideBySideDiff = None
            def analysis = None
          }
      }

    it("can be used with all(a) shouldEqual (b) syntax") {
      val e = intercept[TestFailedException] {
        all(List("test", "test2", "test")) shouldEqual ("test")
      }
      assert(e.message == Some("'all' inspection failed, because: \n" +
        "  at index 1, \"**test[2]\" did not equal \"test[]**\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
        "in List(\"test\", \"test2\", \"test\")"))
      assert(e.differences.flatMap(_.inlineDiff) == Vector(("**test[2]", "test[]**")))
    }

    it("can be used with a shouldEqual (b) syntax") {
      val e = intercept[TestFailedException] {
        "test2" shouldEqual ("test")
      }
      assert(e.message == Some("\"**test[2]\" did not equal \"test[]**\""))
      assert(e.differences.flatMap(_.inlineDiff) == Vector(("**test[2]", "test[]**")))
    }

    it("can be used with a should equal (b) syntax") {
      val e = intercept[TestFailedException] {
        "test2" should equal ("test")
      }
      assert(e.message == Some("\"**test[2]\" did not equal \"test[]**\""))
      assert(e.differences.flatMap(_.inlineDiff) == Vector(("**test[2]", "test[]**")))
    }

  }

}