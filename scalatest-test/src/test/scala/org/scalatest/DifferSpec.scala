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

    describe("when used with default Equality") {

      it("should include differences in TestFailedException thrown from a shouldEqual (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          "test2" shouldEqual ("test")
        }
        assert(e.message == Some("\"test[2]\" did not equal \"test[]\""))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test[2]", "test[]")))
      }

      it("should not include differences in TestFailedException thrown from a shouldEqual (b) syntax when a is String and b is Int") {
        val e = intercept[TestFailedException] {
          "test2" shouldEqual (3)
        }
        assert(e.message == Some("\"test2\" did not equal 3"))
        assert(e.differences.isEmpty)
      }

      it("should not include differences in TestFailedException thrown from a shouldEqual (b) syntax when a is Int and b is String") {
        val e = intercept[TestFailedException] {
          3 shouldEqual ("test2")
        }
        assert(e.message == Some("3 did not equal \"test2\""))
        assert(e.differences.isEmpty)
      }

      it("should not include differences in TestFailedException thrown from a shouldEqual (b) syntax when a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          3 shouldEqual (2)
        }
        assert(e.message == Some("3 did not equal 2"))
        assert(e.differences.isEmpty)
      }

      it("should include differences in TestFailedException thrown from a should equal (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          "test2" should equal("test")
        }
        assert(e.message == Some("\"test[2]\" did not equal \"test[]\""))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test[2]", "test[]")))
      }

      it("should not include differences in TestFailedException thrown from a should equal (b) syntax when a is String and b is Int") {
        val e = intercept[TestFailedException] {
          "test2" should equal (2)
        }
        assert(e.message == Some("\"test2\" did not equal 2"))
        assert(e.differences.isEmpty)
      }

      it("should not include differences in TestFailedException thrown from a should equal (b) syntax when a is Int and b is String") {
        val e = intercept[TestFailedException] {
          2 should equal ("test2")
        }
        assert(e.message == Some("2 did not equal \"test2\""))
        assert(e.differences.isEmpty)
      }

      it("should not include differences in TestFailedException thrown from a should equal (b) syntax when a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          2 should equal (3)
        }
        assert(e.message == Some("2 did not equal 3"))
        assert(e.differences.isEmpty)
      }

      it("should include diffences in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) shouldEqual ("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"test[2]\" did not equal \"test[]\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test[2]", "test[]")))
      }

      it("should not include diffences in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a is String and b is Int") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) shouldEqual (2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, \"test\" did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.isEmpty)
      }

      it("should not include diffences in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a is Int and b is String") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) shouldEqual ("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal \"test\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.differences.isEmpty)
      }

      it("should not include diffences in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) shouldEqual (2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.differences.isEmpty)
      }

      it("should include differences in TestFailedException thrown from all(a) should equal (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) should equal("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"test[2]\" did not equal \"test[]\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("test[2]", "test[]")))
      }

      it("should not include differences in TestFailedException thrown from all(a) should equal (b) syntax when a is String and b is Int") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) should equal(2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, \"test\" did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.isEmpty)
      }

      it("should not include differences in TestFailedException thrown from all(a) should equal (b) syntax when a is Int and b is String") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) should equal("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal \"test\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.differences.isEmpty)
      }

      it("should not include differences in TestFailedException thrown from all(a) should equal (b) syntax when a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) should equal(2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.differences.isEmpty)
      }

    }

    describe("when used with custom Equality that has custom difference implementation") {

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

      it("can be used with a shouldEqual (b) syntax") {
        val e = intercept[TestFailedException] {
          "test2" shouldEqual ("test")
        }
        assert(e.message == Some("\"**test[2]\" did not equal \"test[]**\""))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("**test[2]", "test[]**")))
      }

      it("can be used with a should equal (b) syntax") {
        val e = intercept[TestFailedException] {
          "test2" should equal("test")
        }
        assert(e.message == Some("\"**test[2]\" did not equal \"test[]**\""))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("**test[2]", "test[]**")))
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

      it("can be used with all(a) should equal (b) syntax") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) should equal("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"**test[2]\" did not equal \"test[]**\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.differences.flatMap(_.inlineDiff) == Vector(("**test[2]", "test[]**")))
      }
    }

  }

  val EOL = scala.compat.Platform.EOL

  sealed trait Parent
  case class Bar( s: String, i: Int ) extends Parent
  case class Foo( bar: Bar, b: List[Int], parent: Option[Parent] ) extends Parent

  describe("CaseClassDiffer") {

    it("should produce difference of 2 Bars correctly") {
      val a = Bar("asdf", 5)
      val b = Bar("asdf", 6)
      val c = Bar("asf", 6)

      assert(CaseClassDiffer.difference(a, b).analysis == Some("(i: 5 -> 6)"))
      assert(CaseClassDiffer.difference(b, c).analysis == Some("(s: as[d]f -> as[]f)"))
      assert(CaseClassDiffer.difference(a, c).analysis == Some("(i: 5 -> 6" + EOL + "s: as[d]f -> as[]f)"))
    }

    it("should produce difference of 2 Foos correctly") {
      val a: Foo = Foo(
        Bar( "asdf", 5 ),
        List( 123, 1234 ),
        Some( Bar( "asdf", 5 ) )
      )
      val b: Foo = Foo(
        Bar( "asdf", 66 ),
        List( 1234 ),
        Some( Bar( "qwer", 5 ) )
      )

      println("###difference: ")
      println(CaseClassDiffer.difference(a, b).analysis.get)
    }

  }

}