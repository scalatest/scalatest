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
package org.scalatest

import SharedHelpers.thisLineNumber
import java.util.Date
import org.scalautils.Prettifier

class DiagrammedAssertionsSpec extends FunSpec with Matchers with DiagrammedAssertions {

  val fileName: String = "DiagrammedAssertionsSpec.scala"

  class Stateful {
    var state = false
    def changeState: Boolean = {
      state = true
      state
    }
  }

  class CustomInt(value: Int) {

    def startsWith(v: Int): Boolean = {
      value.toString.startsWith(v.toString)
    }

    def endsWith(v: Int): Boolean = {
      value.toString.endsWith(v.toString)
    }

    def contains(v: Int): Boolean = {
      value.toString.contains(v.toString)
    }

    def exists(v: Int): Boolean = {
      value == v
    }

    override def toString: String = value.toString
  }

  class CustomContainer[+E](e: E) {
    val element: E = e

    def contains[E1 >: E](elem: E1): Boolean = elem == element
  }

  private def neverRuns1(f: => Unit): Boolean = true
  private def neverRuns2(f: => Unit)(a: Int): Boolean = true
  private def neverRuns3[T](f: => Unit)(a: T): Boolean = true

  val s1 = "hi ScalaTest"
  val s2 = "ScalaTest hi"
  val s3 = "Say hi to ScalaTest"
  val s4 = ""

  val ci1 = new CustomInt(123)
  val ci1Str = Prettifier.default(ci1)
  val ci2 = new CustomInt(321)
  val ci2Str = Prettifier.default(ci2)
  val ci3 = ci1
  val ci3Str = Prettifier.default(ci3)

  val l1 = List(1, 2, 3)
  val l2 = List.empty[Int]
  val l3 = List("one", "two", "three")

  val m1 = Map(1 -> "one", 2 -> "two", 3 -> "three")
  val m1Str = Prettifier.default(m1)
  val m2 = Map.empty[Int, String]

  val ct1 = new CustomContainer(8)
  val ct1Str = Prettifier.default(ct1)

  val date = new Date

  def woof(f: => Unit) = "woof"
  def meow(x: Int = 0, y: Int = 3) = "meow"

  describe("DiagrammedAssertions") {

    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should do nothing when is used to check a == 3") {
      assert(a == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 5") {
      val e = intercept[TestFailedException] {
        assert(a == 5)
      }
      e.message should be (
        Some(
          """
            |assert(a == 5)
            |       | |  |
            |       3 |  5
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check 5 == b") {
      assert(5 == b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 == b") {
      val e = intercept[TestFailedException] {
        assert(3 == b)
      }
      e.message should be (
        Some(
          """
            |assert(3 == b)
            |       | |  |
            |       3 |  5
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check a != 5") {
      assert(a != 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a != 3") {
      val e = intercept[TestFailedException] {
        assert(a != 3)
      }
      e.message should be (
        Some(
          """
            |assert(a != 3)
            |       | |  |
            |       3 |  3
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check 3 != b") {
      assert(3 != b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 != b") {
      val e = intercept[TestFailedException] {
        assert(5 != b)
      }
      e.message should be (
        Some(
          """
            |assert(5 != b)
            |       | |  |
            |       5 |  5
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check 3 == 3") {
      assert(3 == 3)
    }

    it("should throw TestFailedException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e1 = intercept[TestFailedException] {
        assert(3 == 5)
      }
      e1.message should be (
        Some(
          """
            |assert(3 == 5)
            |         |
            |         false
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 12))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == b") {
      val e = intercept[TestFailedException] {
        assert(a == b)
      }
      e.message should be (
        Some(
          """
            |assert(a == b)
            |       | |  |
            |       3 |  5
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == null") {
      val e = intercept[TestFailedException] {
        assert(a == null)
      }
      e.message should be (
        Some(
          """
            |assert(a == null)
            |       | |  |
            |       3 |  null
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val e = intercept[TestFailedException] {
        assert(null == a)
      }
      e.message should be (
        Some(
          """
            |assert(null == a)
            |       |    |  |
            |       null |  3
            |            false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
      val e = intercept[TestFailedException] {
        assert(3 != a)
      }
      e.message should be (
        Some(
          """
            |assert(3 != a)
            |       | |  |
            |       3 |  3
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check 5 != a") {
      assert(5 != a)
    }

    it("should do nothing when is used to check a > 2") {
      assert(a > 2)
    }

    it("should do nothing when is used to check 5 > a") {
      assert(5 > a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[TestFailedException] {
        assert(a > 3)
      }
      e.message should be (
        Some(
          """
            |assert(a > 3)
            |       | | |
            |       3 | 3
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[TestFailedException] {
        assert(3 > a)
      }
      e.message should be (
        Some(
          """
            |assert(3 > a)
            |       | | |
            |       3 | 3
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check a >= 3") {
      assert(a >= 3)
    }

    it("should do nothing when is used to check 3 >= a") {
      assert(3 >= a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[TestFailedException] {
        assert(a >= 4)
      }
      e.message should be (
        Some(
          """
            |assert(a >= 4)
            |       | |  |
            |       3 |  4
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[TestFailedException] {
        assert(2 >= a)
      }
      e.message should be (
        Some(
          """
            |assert(2 >= a)
            |       | |  |
            |       2 |  3
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check b < 6") {
      assert(b < 6)
    }

    it("should do nothing when is used to check 3 < b") {
      assert(3 < b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[TestFailedException] {
        assert(b < 5)
      }
      e.message should be (
        Some(
          """
            |assert(b < 5)
            |       | | |
            |       5 | 5
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[TestFailedException] {
        assert(5 < b)
      }
      e.message should be (
        Some(
          """
            |assert(5 < b)
            |       | | |
            |       5 | 5
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check b <= 5") {
      assert(b <= 5)
    }

    it("should do nothing when is used to check 5 <= b") {
      assert(5 <= b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[TestFailedException] {
        assert(b <= 4)
      }
      e.message should be (
        Some(
          """
            |assert(b <= 4)
            |       | |  |
            |       5 |  4
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[TestFailedException] {
        assert(6 <= b)
      }
      e.message should be (
        Some(
          """
            |assert(6 <= b)
            |       | |  |
            |       6 |  5
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check bob == \"bob\"") {
      assert(bob == "bob")
    }

    it("should do nothing when is used to check bob != \"alice\"") {
      assert(bob != "alice")
    }

    it("should do nothing when is used to check alice == \"alice\"") {
      assert(alice == "alice")
    }

    it("should do nothing when is used to check alice != \"bob\"") {
      assert(alice != "bob")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check bob == \"alice\"") {
      val e = intercept[TestFailedException] {
        assert(bob == "alice")
      }
      e.message should be (
        Some(
          """
            |assert(bob == "alice")
            |       |   |  |
            |       |   |  "alice"
            |       |   false
            |       "bob"
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check bob != \"bob\"") {
      val e = intercept[TestFailedException] {
        assert(bob != "bob")
      }
      e.message should be (
        Some(
          """
            |assert(bob != "bob")
            |       |   |  |
            |       |   |  "bob"
            |       |   false
            |       "bob"
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check alice == \"bob\"") {
      val e = intercept[TestFailedException] {
        assert(alice == "bob")
      }
      e.message should be (
        Some(
          """
            |assert(alice == "bob")
            |       |     |  |
            |       |     |  "bob"
            |       |     false
            |       "alice"
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check alice != \"alice\"") {
      val e = intercept[TestFailedException] {
        assert(alice != "alice")
      }
      e.message should be (
        Some(
          """
            |assert(alice != "alice")
            |       |     |  |
            |       |     |  "alice"
            |       |     false
            |       "alice"
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check a === 3") {
      assert(a === 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5 ") {
      val e = intercept[TestFailedException] {
        assert(a === 5)
      }
      e.message should be (
        Some(
          """
            |assert(a === 5)
            |       | |   |
            |       3 |   5
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check 3 === a") {
      assert(3 === a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
      val e = intercept[TestFailedException] {
        assert(5 === a)
      }
      e.message should be (
        Some(
          """
            |assert(5 === a)
            |       | |   |
            |       5 |   3
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check a !== 5") {
      assert(a !== 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a !== 3") {
      val e = intercept[TestFailedException] {
        assert(a !== 3)
      }
      e.message should be (
        Some(
          """
            |assert(a !== 3)
            |       | |   |
            |       3 |   3
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check 5 !== a") {
      assert(5 !== a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 !== a") {
      val e = intercept[TestFailedException] {
        assert(3 !== a)
      }
      e.message should be (
        Some(
          """
            |assert(3 !== a)
            |       | |   |
            |       3 |   3
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      assert(a == 3 && b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && b == 6)
      }
      e.message should be (
        Some(
          """
            |assert(a == 3 && b == 6)
            |       | |  | |  | |  |
            |       3 |  3 |  5 |  6
            |         true |    false
            |              false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[TestFailedException] {
        assert(a == 2 && b == 5)
      }
      e.message should be (
        Some(
          """
            |assert(a == 2 && b == 5)
            |       | |  | |  | |  |
            |       3 |  2 |  5 |  5
            |         |    |    true
            |         |    false
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 && b == 6)
      }
      e.message should be (
        Some(
          """
            |assert(a == 2 && b == 6)
            |       | |  | |  | |  |
            |       3 |  2 |  5 |  6
            |         |    |    false
            |         |    false
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      assert(a == 3 & b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 3 & b == 6)
      }
      e.message should be (
        Some(
          """
            |assert(a == 3 & b == 6)
            |       | |  | | | |  |
            |       3 |  3 | 5 |  6
            |         true |   false
            |              false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[TestFailedException] {
        assert(a == 2 & b == 5)
      }
      e.message should be (
        Some(
          """
            |assert(a == 2 & b == 5)
            |       | |  | | | |  |
            |       3 |  2 | 5 |  5
            |         |    |   true
            |         |    false
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 & b == 6)
      }
      e.message should be (
        Some(
          """
            |assert(a == 2 & b == 6)
            |       | |  | | | |  |
            |       3 |  2 | 5 |  6
            |         |    |   false
            |         |    false
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      assert(a == 3 || b == 5)
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      assert(a == 3 || b == 6)
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      assert(a == 2 || b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 || b == 6)
      }
      e.message should be (
        Some(
          """
            |assert(a == 2 || b == 6)
            |       | |  | |  | |  |
            |       3 |  2 |  5 |  6
            |         |    |    false
            |         |    false
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      assert(a == 3 | b == 5)
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      assert(a == 3 | b == 6)
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      assert(a == 2 | b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 | b == 6)
      }
      e.message should be (
        Some(
          """
            |assert(a == 2 | b == 6)
            |       | |  | | | |  |
            |       3 |  2 | 5 |  6
            |         |    |   false
            |         |    false
            |         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      assert(a == 3 && (b == 5 && b > 3))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && (b == 5 && b > 5))
      }
      e.message should be (
        Some(
          """
            |assert(a == 3 && (b == 5 && b > 5))
            |       | |  | |   | |  | |  | | |
            |       3 |  3 |   5 |  5 |  5 | 5
            |         true false true |    false
            |                         false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check !(a == 5)") {
      assert(!(a == 5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[TestFailedException] {
        assert(!(a == 3))
      }
      e.message should be (
        Some(
          """
            |assert(!(a == 3))
            |       | | |  |
            |       | 3 |  3
            |       |   true
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && !(b == 5))
      }
      e.message should be (
        Some(
          """
            |assert(a == 3 && !(b == 5))
            |       | |  | |  | | |  |
            |       3 |  3 |  | 5 |  5
            |         true |  |   true
            |              |  false
            |              false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      assert((a == 3) == (b == 5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[TestFailedException] {
        assert((a == 3) == (b != 5))
      }
      e.message should be (
        Some(
          """
            |assert((a == 3) == (b != 5))
            |        | |  |  |   | |  |
            |        3 |  3  |   5 |  5
            |          true  false false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    /*it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        assert(a == 5 && s.changeState)
      }
      s.state should be (false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      assert(a == 3 || s.changeState)
      s.state should be (false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      assert(a == 3 | s.changeState)
      s.state should be (false)
    }*/

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      assert(a == 3 && { println("hi"); b == 5})
    }

    /*it("should throw TestFailedException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && { println("hi"); b == 3})
      }
      e.message should be (
        Some(
          """
            |assert(a == 3 && { println("hi"); b == 3})
            |       | |  | |                   | |  |
            |       3 |  3 |                   5 |  3
            |         true false                 false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }*/

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
      assert({ println("hi"); b == 5} && a == 3)
    }

    /*it("should throw TestFailedException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestFailedException] {
        assert({ println("hi"); b == 5} && a == 5)
      }
      e.message should be (
        Some(
          """
            |assert({ println("hi"); b == 5} && a == 5)
            |                        | |  |  |  | |  |
            |                        5 |  5  |  3 |  5
            |                          true  |    false
            |                                true
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }*/

    /*it("should preserve side effects when Apply with single argument is passed in") {
      assert(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      assert(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      assert(neverRuns3(sys.error("Sad times 3"))(0))
    }*/

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      assert(s1 startsWith "hi")
      assert(s1.startsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[TestFailedException] {
        assert(s2 startsWith "hi")
      }
      e1.message should be (
        Some(
          """
            |assert(s2 startsWith "hi")
            |       |  |          |
            |       |  false      "hi"
            |       "ScalaTest hi"
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

      val e2 = intercept[TestFailedException] {
        assert(s2.startsWith("hi"))
      }
      e2.message should be (
        Some(
          """
            |assert(s2.startsWith("hi"))
            |       |  |          |
            |       |  false      "hi"
            |       "ScalaTest hi"
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      assert(ci1 startsWith 1)
      assert(ci1.startsWith(1))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[TestFailedException] {
        assert(ci2 startsWith 1)
      }
      e1.message should be (
        Some(
          """
            |assert(ci2 startsWith 1)
            |       |   |          |
            |       321 false      1
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 12))

      val e2 = intercept[TestFailedException] {
        assert(ci2.startsWith(1))
      }
      e2.message should be (
        Some(
          """
            |assert(ci2.startsWith(1))
            |       |   |          |
            |       321 false      1
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 12))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      assert(!s2.startsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        assert(!s1.startsWith("hi"))
      }
      e1.message should be (
        Some(
          """
            |assert(!s1.startsWith("hi"))
            |       ||  |          |
            |       ||  true       "hi"
            |       |"hi ScalaTest"
            |       false
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      assert(s2 endsWith "hi")
      assert(s2.endsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[TestFailedException] {
        assert(s1 endsWith "hi")
      }
      e1.message should be (
        Some(
          """
            |assert(s1 endsWith "hi")
            |       |  |        |
            |       |  false    "hi"
            |       "hi ScalaTest"
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

      val e2 = intercept[TestFailedException] {
        assert(s1.endsWith("hi"))
      }
      e2.message should be (
        Some(
          """
            |assert(s1.endsWith("hi"))
            |       |  |        |
            |       |  false    "hi"
            |       "hi ScalaTest"
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      assert(ci2 endsWith 1)
      assert(ci2.endsWith(1))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 endsWith 1)
      }
      e1.message should be (
        Some(
          """
            |assert(ci1 endsWith 1)
            |       |   |        |
            |       123 false    1
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 12))

      val e2 = intercept[TestFailedException] {
        assert(ci1.endsWith(1))
      }
      e2.message should be (
        Some(
          """
            |assert(ci1.endsWith(1))
            |       |   |        |
            |       123 false    1
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 12))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      assert(!s1.endsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        assert(!s2.endsWith("hi"))
      }
      e1.message should be (
        Some(
          """
            |assert(!s2.endsWith("hi"))
            |       ||  |        |
            |       ||  true     "hi"
            |       |"ScalaTest hi"
            |       false
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      assert(s3 contains "hi")
      assert(s3.contains("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[TestFailedException] {
        assert(s3 contains "hello")
      }
      e1.message should be (
        Some(
          """
            |assert(s3 contains "hello")
            |       |  |        |
            |       |  false    "hello"
            |       "Say hi to ScalaTest"
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

      val e2 = intercept[TestFailedException] {
        assert(s3.contains("hello"))
      }
      e2.message should be (
        Some(
          """
            |assert(s3.contains("hello"))
            |       |  |        |
            |       |  false    "hello"
            |       "Say hi to ScalaTest"
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      assert(ci2 contains 2)
      assert(ci2.contains(2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 contains 5)
      }
      e1.message should be (
        Some(
          """
            |assert(ci1 contains 5)
            |       |   |        |
            |       123 false    5
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 12))

      val e2 = intercept[TestFailedException] {
        assert(ci1.contains(5))
      }
      e2.message should be (
        Some(
          """
            |assert(ci1.contains(5))
            |       |   |        |
            |       123 false    5
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 12))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      assert(!s3.contains("hello"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        assert(!s3.contains("hi"))
      }
      e1.message should be (
        Some(
          """
            |assert(!s3.contains("hi"))
            |       ||  |        |
            |       ||  true     "hi"
            |       |"Say hi to ScalaTest"
            |       false
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check l1 contains 2") {
      assert(l1 contains 2)
      assert(l1.contains(2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(l1 contains 5)
      }
      e1.message should be (
        Some(
          """
            |assert(l1 contains 5)
            |       |  |        |
            |       |  false    5
            |       List(1, 2, 3)
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

      val e2 = intercept[TestFailedException] {
        assert(l1.contains(5))
      }
      e2.message should be (
        Some(
          """
            |assert(l1.contains(5))
            |       |  |        |
            |       |  false    5
            |       List(1, 2, 3)
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      assert(!(l1 contains 5))
      assert(!l1.contains(5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[TestFailedException] {
        assert(!(l1 contains 2))
      }
      e1.message should be (
        Some(
          """
            |assert(!(l1 contains 2))
            |       | |  |        |
            |       | |  true     2
            |       | List(1, 2, 3)
            |       false
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

      val e2 = intercept[TestFailedException] {
        assert(!l1.contains(2))
      }
      e2.message should be (
        Some(
          """
            |assert(!l1.contains(2))
            |       ||  |        |
            |       ||  true     2
            |       |List(1, 2, 3)
            |       false
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check m1 contains 2") {
      assert(m1 contains 2)
      assert(m1.contains(2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(m1 contains 5)
      }
      e1.message should be (
        Some(
          s"""
            |assert(m1 contains 5)
            |       |  |        |
            |       |  false    5
            |       $m1Str
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

      val e2 = intercept[TestFailedException] {
        assert(m1.contains(5))
      }
      e2.message should be (
        Some(
          s"""
            |assert(m1.contains(5))
            |       |  |        |
            |       |  false    5
            |       $m1Str
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      assert(!(m1 contains 5))
      assert(!m1.contains(5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[TestFailedException] {
        assert(!(m1 contains 2))
      }
      e1.message should be (
        Some(
          s"""
            |assert(!(m1 contains 2))
            |       | |  |        |
            |       | |  true     2
            |       | $m1Str
            |       false
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

      val e2 = intercept[TestFailedException] {
        assert(!m1.contains(2))
      }
      e2.message should be (
        Some(
          s"""
            |assert(!m1.contains(2))
            |       ||  |        |
            |       ||  true     2
            |       |$m1Str
            |       false
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      assert(ct1 contains 8)
      assert(ct1.contains(8))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(ct1 contains 5)
      }
      e1.message should be (
        Some(
          s"""
            |assert(ct1 contains 5)
            |       |   |        |
            |       |   false    5
            |       $ct1Str
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

      val e2 = intercept[TestFailedException] {
        assert(ct1.contains(5))
      }
      e2.message should be (
        Some(
          s"""
            |assert(ct1.contains(5))
            |       |   |        |
            |       |   false    5
            |       $ct1Str
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      assert(!ct1.contains(5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[TestFailedException] {
        assert(!ct1.contains(8))
      }
      e1.message should be (
        Some(
          s"""
            |assert(!ct1.contains(8))
            |       ||   |        |
            |       ||   true     8
            |       |$ct1Str
            |       false
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      assert(ci1 eq ci3)
      assert(ci1.eq(ci3))
    }

    /*it("should throw TestFailedException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 eq ci2)
      }
      e1.message should be (
        Some(
          s"""
            |assert(ci1 eq ci2)
            |       |   |  |
            |       |   |  $ci2Str
            |       |   false
            |       $ci1Str
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

      val e2 = intercept[TestFailedException] {
        assert(ci1.eq(ci2))
      }
      e2.message should be (
        Some(
          s"""
            |assert(ci1.eq(ci2))
            |       |   |  |
            |       |   |  $ci2Str
            |       |   false
            |       $ci1Str
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }*/

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      assert(!ci1.eq(ci2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[TestFailedException] {
        assert(!ci1.eq(ci3))
      }
      e.message should be (
        Some(
          s"""
            |assert(!ci1.eq(ci3))
            |       ||   |  |
            |       |$ci1Str |  $ci3Str
            |       |    true
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      assert(ci1 ne ci2)
      assert(ci1.ne(ci2))
    }

    /*it("should throw TestFailedException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 ne ci3)
      }
      e1.message should be (
        Some(
          s"""
            |assert(ci1 ne ci3)
            |       |   |  |
            |       $ci1Str |  $ci3Str
            |           false
            |""".stripMargin
        )
      )
      e1.failedCodeFileName should be (Some(fileName))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

      val e2 = intercept[TestFailedException] {
        assert(ci1.ne(ci3))
      }
      e2.message should be (
        Some(
          s"""
            |assert(ci1.ne(ci3))
            |       |   |  |
            |       $ci1Str |  $ci3Str
            |           false
            |""".stripMargin
        )
      )
      e2.failedCodeFileName should be (Some(fileName))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }*/

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      assert(!ci1.ne(ci3))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[TestFailedException] {
        assert(!ci1.ne(ci2))
      }
      e.message should be (
        Some(
          """
            |assert(!ci1.ne(ci2))
            |       ||   |  |
            |       |123 |  321
            |       |    true
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      assert(s4.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(s3.isEmpty)
      }
      e.message should be (
        Some(
          """
            |assert(s3.isEmpty)
            |       |  |
            |       |  false
            |       "Say hi to ScalaTest"
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      assert(!s3.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(!s4.isEmpty)
      }
      e.message should be (
        Some(
          """
            |assert(!s4.isEmpty)
            |       ||  |
            |       |"" true
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      assert(l2.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(l1.isEmpty)
      }
      e.message should be (
        Some(
          s"""
            |assert(l1.isEmpty)
            |       |  |
            |       |  false
            |       $l1
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      assert(!l1.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(!l2.isEmpty)
      }
      e.message should be (
        Some(
          s"""
            |assert(!l2.isEmpty)
            |       ||  |
            |       ||  true
            |       |$l2
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      assert(s1.isInstanceOf[String])
    }

    /*it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[TestFailedException] {
        assert(l1.isInstanceOf[String])
      }
      e.message should be (
        Some(
          s"""
            |assert(l1.isInstanceOf[String])
            |       |  |
            |       |  false
            |       $l1
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }*/

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      assert(l1.isInstanceOf[List[Int]])
    }

    /*it("should throw TestFailedException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[TestFailedException] {
        assert(s1.isInstanceOf[List[Int]])
      }
      e.message should be (
        Some(
          """
            |assert(s1.isInstanceOf[List[Int]])
            |       |  |
            |       |  false
            |       "hi ScalaTest"
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }*/

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      assert(date.isInstanceOf[Date])
    }

    /*it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[TestFailedException] {
        assert(l1.isInstanceOf[Date])
      }
      e.message should be (
        Some(
          s"""
            |assert(l1.isInstanceOf[Date])
            |       |  |
            |       |  false
            |       $l1
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }*/

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      assert(!l1.isInstanceOf[String])
    }

    /*it("should throw TestFailedException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[TestFailedException] {
        assert(!s1.isInstanceOf[String])
      }
      e.message should be (
        Some(
          """
            |assert(!s1.isInstanceOf[String])
            |       ||  |
            |       ||  true
            |       |"hi ScalaTest"
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }*/

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      assert(!s1.isInstanceOf[List[Int]])
    }

    /*it("should throw TestFailedException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[TestFailedException] {
        assert(!l1.isInstanceOf[List[Int]])
      }
      e.message should be (
        Some(
          s"""
            |assert(!l1.isInstanceOf[List[Int]])
            |       ||  |
            |       ||  true
            |       |$l1
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }*/

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      assert(!l1.isInstanceOf[Date])
    }

    /*it("should throw TestFailedException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[TestFailedException] {
        assert(!date.isInstanceOf[Date])
      }
      e.message should be (
        Some(
          s"""
            |assert(!date.isInstanceOf[Date])
            |       ||  |
            |       ||  true
            |       |$date
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }*/

    it("should do nothing when is used to check s1.length == 9") {
      assert(s1.length == 12)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[TestFailedException] {
        assert(s1.length == 10)
      }
      e.message should be (
        Some(
          """
            |assert(s1.length == 10)
            |       |  |      |  |
            |       |  12     |  10
            |       |         false
            |       "hi ScalaTest"
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check l1.length == 3") {
      assert(l1.length == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[TestFailedException] {
        assert(l1.length == 10)
      }
      e.message should be (
        Some(
          s"""
            |assert(l1.length == 10)
            |       |  |      |  |
            |       |  3      |  10
            |       |         false
            |       $l1
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      assert(!(s1.length == 10))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(s1.length == 12))
      }
      e.message should be (
        Some(
          """
            |assert(!(s1.length == 12))
            |       | |  |      |  |
            |       | |  12     |  12
            |       | |         true
            |       | "hi ScalaTest"
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      assert(!(l1.length == 2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(l1.length == 3))
      }
      e.message should be (
        Some(
          s"""
            |assert(!(l1.length == 3))
            |       | |  |      |  |
            |       | |  3      |  3
            |       | |         true
            |       | $l1
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }

    /*it("should do nothing when is used to check s1.size == 9") {
      assert(s1.size == 12)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[TestFailedException] {
        assert(s1.size == 10)
      }
      e.message should be (
        Some(
          """
            |assert(s1.size == 10)
            |       |  |    |  |
            |       |  12   |  10
            |       |       false
            |       "hi ScalaTest"
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }*/

    it("should do nothing when is used to check l1.size == 3") {
      assert(l1.size == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[TestFailedException] {
        assert(l1.size == 10)
      }
      e.message should be (
        Some(
          s"""
            |assert(l1.size == 10)
            |       |  |    |  |
            |       |  3    |  10
            |       |       false
            |       $l1
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }

    /*it("should do nothing when is used to check !(s1.size == 10)") {
      assert(!(s1.size == 10))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(s1.size == 12))
      }
      e.message should be (
        Some(
          """
            |assert(!(s1.size == 12))
            |       | |  |    |  |
            |       | |  12   |  12
            |       | |       true
            |       | "hi ScalaTest"
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }*/

    it("should do nothing when is used to check !(l1.size == 2)") {
      assert(!(l1.size == 2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(l1.size == 3))
      }
      e.message should be (
        Some(
          s"""
            |assert(!(l1.size == 3))
            |       | |  |    |  |
            |       | |  3    |  3
            |       | |       true
            |       | $l1
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
    }

    /*it("should do nothing when is used to check l1.exists(_ == 3)") {
      assert(l1.exists(_ == 3))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(_ == 5))
      }
      e.message should be (
        Some(
          s"""
            |assert(l1.exists(_ == 5))
            |       |  |
            |       |  false
            |       $l1
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }*/

    /*it("should do nothing when is used to check !l1.exists(_ == 5)") {
      assert(!l1.exists(_ == 5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestFailedException] {
        assert(!l1.exists(_ == 3))
      }
      e.message should be (
        Some(
          s"""
            |assert(!l1.exists(_ == 3))
            |       ||  |
            |       ||  true
            |       |$l1
            |       false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
    }*/

    /*it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(_ > 3))
      }
      e.message should be (
        Some(
          s"""
            |assert(l1.exists(_ > 3))
            |       |  |
            |       |  false
            |       $l1
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestFailedException] {
        assert(l3.exists(_.isEmpty))
      }
      e.message should be (
        Some(
          s"""
            |assert(l3.exists(_.isEmpty))
            |       |  |
            |       |  false
            |       $l3
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }*/

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[TestFailedException] {
        assert(ci1.exists(321))
      }
      e.message should be (
        Some(
          """
            |assert(ci1.exists(321))
            |       |   |      |
            |       123 false  321
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 12))
    }

    /*it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      assert(woof { meow(y = 5) } == "woof")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[TestFailedException] {
        assert(woof { meow(y = 5) } == "meow")
      }
      e.message should be (
        Some(
          """
            |assert(woof { meow(y = 5) } == "meow")
            |       |                    |  |
            |       "meow"               |  "meow"
            |                            false
            |""".stripMargin
        )
      )
      e.failedCodeFileName should be (Some(fileName))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
    }*/

  }

}