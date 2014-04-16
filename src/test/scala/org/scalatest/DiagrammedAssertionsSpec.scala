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

class DiagrammedAssertionsSpec extends FunSpec with Matchers with DiagrammedAssertions {

  val fileName: String = "DiagrammedAssertionsSpec.scala"

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

  }

}