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
package org.scalatest.diagrams

import org.scalatest.SharedHelpers.thisLineNumber
import java.util.Date
import org.scalactic.Prettifier
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec

class DirectDiagrammedAssertionsSpec extends AnyFunSpec with org.scalatest.matchers.should.Matchers {

  val fileName: String = "DirectDiagrammedAssertionsSpec.scala"

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
  val l3Str = Prettifier.default(l3)

  val m1 = Map(1 -> "one", 2 -> "two", 3 -> "three")
  val m1Str = Prettifier.default(m1)
  val m2 = Map.empty[Int, String]

  val ct1 = new CustomContainer(8)
  val ct1Str = Prettifier.default(ct1)

  val date = new Date

  def woof(f: => Unit) = "woof"
  def meow(x: Int = 0, y: Int = 3) = "meow"

  def varargs(x: Int, y: String*): (Int, Seq[String]) = (x, y.toSeq)
  class CustomList(value: List[Int]) {
    def contains(x: Int*): Boolean = x.forall(value.contains(_))
  }
  val cList1 = new CustomList(List(1, 2, 3))
 
  describe("DiagrammedAssertions") {

    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    describe("The org.scalatest.diagrams.Diagrams.assert(boolean) method") {
      it("should do nothing when is used to check a == 3") {
        org.scalatest.diagrams.Diagrams.assert(a == 3)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 5") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 5)
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 == b") {
        org.scalatest.diagrams.Diagrams.assert(5 == b)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 3 == b") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(3 == b)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(3 == b)
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a != 5") {
        org.scalatest.diagrams.Diagrams.assert(a != 5)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a != 3") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a != 3)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a != 3)
              |                                       | |  |
              |                                       3 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 != b") {
        org.scalatest.diagrams.Diagrams.assert(3 != b)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 5 != b") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(5 != b)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(5 != b)
              |                                       | |  |
              |                                       5 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 == 3") {
        org.scalatest.diagrams.Diagrams.assert(3 == 3)
      }

      // SKIP-DOTTY-START
      // const-folding will eliminate the expr
      it("should throw TestFailedException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
        // This is because the compiler simply pass the false boolean literal
        // to the macro, can't find a way to get the 3 == 5 literal.
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(3 == 5)
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(3 == 5)
              |                                         |
              |                                         false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }
      // SKIP-DOTTY-END

      it("should throw TestFailedException with correct message and stack depth when is used to check a == b") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == b)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == b)
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      // SKIP-DOTTY-START
      it("should throw TestFailedException with correct message and stack depth when is used to check a == null") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == null)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == null)
              |                                       | |  |
              |                                       3 |  null
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(null == a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(null == a)
              |                                       |    |  |
              |                                       null |  3
              |                                            false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }
      // SKIP-DOTTY-END

      it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(3 != a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(3 != a)
              |                                       | |  |
              |                                       3 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 != a") {
        org.scalatest.diagrams.Diagrams.assert(5 != a)
      }

      it("should do nothing when is used to check a > 2") {
        org.scalatest.diagrams.Diagrams.assert(a > 2)
      }

      it("should do nothing when is used to check 5 > a") {
        org.scalatest.diagrams.Diagrams.assert(5 > a)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a > 3") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a > 3)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a > 3)
              |                                       | | |
              |                                       3 | 3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 3 > a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(3 > a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(3 > a)
              |                                       | | |
              |                                       3 | 3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a >= 3") {
        org.scalatest.diagrams.Diagrams.assert(a >= 3)
      }

      it("should do nothing when is used to check 3 >= a") {
        org.scalatest.diagrams.Diagrams.assert(3 >= a)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a >= 4") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a >= 4)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a >= 4)
              |                                       | |  |
              |                                       3 |  4
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 2 >= a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(2 >= a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(2 >= a)
              |                                       | |  |
              |                                       2 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check b < 6") {
        org.scalatest.diagrams.Diagrams.assert(b < 6)
      }

      it("should do nothing when is used to check 3 < b") {
        org.scalatest.diagrams.Diagrams.assert(3 < b)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check b < 5") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(b < 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(b < 5)
              |                                       | | |
              |                                       5 | 5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 5 < b") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(5 < b)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(5 < b)
              |                                       | | |
              |                                       5 | 5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check b <= 5") {
        org.scalatest.diagrams.Diagrams.assert(b <= 5)
      }

      it("should do nothing when is used to check 5 <= b") {
        org.scalatest.diagrams.Diagrams.assert(5 <= b)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check b <= 4") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(b <= 4)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(b <= 4)
              |                                       | |  |
              |                                       5 |  4
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 6 <= b") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(6 <= b)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(6 <= b)
              |                                       | |  |
              |                                       6 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check bob == \"bob\"") {
        org.scalatest.diagrams.Diagrams.assert(bob == "bob")
      }

      it("should do nothing when is used to check bob != \"alice\"") {
        org.scalatest.diagrams.Diagrams.assert(bob != "alice")
      }

      it("should do nothing when is used to check alice == \"alice\"") {
        org.scalatest.diagrams.Diagrams.assert(alice == "alice")
      }

      it("should do nothing when is used to check alice != \"bob\"") {
        org.scalatest.diagrams.Diagrams.assert(alice != "bob")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check bob == \"alice\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(bob == "alice")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(bob == "alice")
              |                                       |   |  |
              |                                       |   |  "alice"
              |                                       |   false
              |                                       "bob"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check bob != \"bob\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(bob != "bob")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(bob != "bob")
              |                                       |   |  |
              |                                       |   |  "bob"
              |                                       |   false
              |                                       "bob"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check alice == \"bob\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(alice == "bob")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(alice == "bob")
              |                                       |     |  |
              |                                       |     |  "bob"
              |                                       |     false
              |                                       "alice"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check alice != \"alice\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(alice != "alice")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(alice != "alice")
              |                                       |     |  |
              |                                       |     |  "alice"
              |                                       |     false
              |                                       "alice"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }
      
      it("should do nothing when is used to check Seq(a, b) == Seq(3, 5)") {
        org.scalatest.diagrams.Diagrams.assert(Seq(a, b) == Seq(3, 5))
      }

      it("should throw TestFailedException when is used to check Set(a, b) == Set(4)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(Set(a, b) == Set(4))
        }
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
      }

      it("should do nothing when is used to check varargs(1, y, z) == 1 -> Seq(y, z)") {
        org.scalatest.diagrams.Diagrams.assert(varargs(1, "y", "z") == 1 -> Seq("y", "z"))
      }

      it("should do nothing when is used to check cList1.contains(1, 2)") {
        org.scalatest.diagrams.Diagrams.assert(cList1.contains(1, 2))
      }

      it("should do nothing when is used to check a === 3") {
        org.scalatest.diagrams.Diagrams.assert(a === 3)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a === 5 ") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a === 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a === 5)
              |                                       | |   |
              |                                       3 |   5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 === a") {
        org.scalatest.diagrams.Diagrams.assert(3 === a)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(5 === a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(5 === a)
              |                                       | |   |
              |                                       5 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a !== 5") {
        org.scalatest.diagrams.Diagrams.assert(a !== 5)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a !== 3") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a !== 3)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a !== 3)
              |                                       | |   |
              |                                       3 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 !== a") {
        org.scalatest.diagrams.Diagrams.assert(5 !== a)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 3 !== a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(3 !== a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(3 !== a)
              |                                       | |   |
              |                                       3 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 && b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 && b == 5)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 3 && b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 3 && b == 6)
              |                                       | |  | |  | |  |
              |                                       3 |  3 |  5 |  6
              |                                         true |    false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 5") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 && b == 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 && b == 5)
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 && b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 && b == 6)
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 & b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 & b == 5)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 & b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 3 & b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 3 & b == 6)
              |                                       | |  | | | |  |
              |                                       3 |  3 | 5 |  6
              |                                         true |   false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 5") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 & b == 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 & b == 5)
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 & b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 & b == 6)
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 || b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 || b == 5)
      }

      it("should do nothing when is used to check a == 3 || b == 6") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 || b == 6)
      }

      it("should do nothing when is used to check a == 2 || b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 2 || b == 5)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 || b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 || b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 || b == 6)
              |                                       | |  | |  | |  |
              |                                       3 |  2 |  5 |  6
              |                                         |    |    false
              |                                         |    false
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check a == 3 | b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 | b == 5)
      }

      it("should do nothing when is used to check a == 3 | b == 6") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 | b == 6)
      }

      it("should do nothing when is used to check a == 2 | b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 2 | b == 5)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 | b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 | b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 | b == 6)
              |                                       | |  | | | |  |
              |                                       3 |  2 | 5 |  6
              |                                         |    |   false
              |                                         |    false
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 && (b == 5 && b > 3))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 3 && (b == 5 && b > 5))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 3 && (b == 5 && b > 5))
              |                                       | |  | |   | |  | |  | | |
              |                                       3 |  3 |   5 |  5 |  5 | 5
              |                                         true false true |    false
              |                                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(a == 5)") {
        org.scalatest.diagrams.Diagrams.assert(!(a == 5))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(a == 3)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(a == 3))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!(a == 3))
              |                                       | | |  |
              |                                       | 3 |  3
              |                                       |   true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 3 && !(b == 5))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 3 && !(b == 5))
              |                                       | |  | |  | | |  |
              |                                       3 |  3 |  | 5 |  5
              |                                         true |  |   true
              |                                              |  false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check (a == 3) == (b == 5)") {
        org.scalatest.diagrams.Diagrams.assert((a == 3) == (b == 5))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert((a == 3) == (b != 5))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert((a == 3) == (b != 5))
              |                                        | |  |  |   | |  |
              |                                        3 |  3  |   5 |  5
              |                                          true  false false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should short-circuit && when first condition was false") {
        val s = new Stateful
        intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 5 && s.changeState)
        }
        s.state should be (false)
      }

      it("should short-circuit & when first condition was false") {
        val s = new Stateful
        intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 5 & s.changeState)
        }
        s.state should be (false)
      }

      it("should short-circuit || when first condition was true") {
        val s = new Stateful
        org.scalatest.diagrams.Diagrams.assert(a == 3 || s.changeState)
        s.state should be (false)
      }

      it("should short-circuit | when first condition was true") {
        val s = new Stateful
        org.scalatest.diagrams.Diagrams.assert(a == 3 | s.changeState)
        s.state should be (false)
      }

      it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5} ") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 && { println("hi"); b == 5})
      }

      it("should throw TestFailedException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 3 && { println("hi"); b == 3})
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 3 && { println("hi"); b == 3})
              |                                       | |  | |                   | |  |
              |                                       3 |  3 false               5 |  3
              |                                         true                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
        org.scalatest.diagrams.Diagrams.assert({ println("hi"); b == 5} && a == 3)
      }

      it("should throw TestFailedException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert({ println("hi"); b == 5} && a == 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert({ println("hi"); b == 5} && a == 5)
              |                                                        | |  |  |  | |  |
              |                                                        5 |  5  |  3 |  5
              |                                                          true  |    false
              |                                                                false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should preserve side effects when Apply with single argument is passed in") {
        org.scalatest.diagrams.Diagrams.assert(neverRuns1(sys.error("Sad times 1")))
      }

      it("should preserve side effects when Apply with 2 argument list is passed in") {
        org.scalatest.diagrams.Diagrams.assert(neverRuns2(sys.error("Sad times 2"))(0))
      }

      it("should preserve side effects when typed Apply with 2 argument list is passed in") {
        org.scalatest.diagrams.Diagrams.assert(neverRuns3(sys.error("Sad times 3"))(0))
      }

      it("should do nothing when is used to check s1 startsWith \"hi\"") {
        org.scalatest.diagrams.Diagrams.assert(s1 startsWith "hi")
        org.scalatest.diagrams.Diagrams.assert(s1.startsWith("hi"))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s2 startsWith "hi")
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(s2 startsWith "hi")
              |                                       |  |          |
              |                                       |  false      "hi"
              |                                       "ScalaTest hi"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s2.startsWith("hi"))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(s2.startsWith("hi"))
              |                                       |  |          |
              |                                       |  false      "hi"
              |                                       "ScalaTest hi"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci1 startsWith 1") {
        org.scalatest.diagrams.Diagrams.assert(ci1 startsWith 1)
        org.scalatest.diagrams.Diagrams.assert(ci1.startsWith(1))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ci2 startsWith 1") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci2 startsWith 1)
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(ci2 startsWith 1)
              |                                       |   |          |
              |                                       321 false      1
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci2.startsWith(1))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(ci2.startsWith(1))
              |                                       |   |          |
              |                                       321 false      1
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
        org.scalatest.diagrams.Diagrams.assert(!s2.startsWith("hi"))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!s1.startsWith("hi"))
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!s1.startsWith("hi"))
              |                                       ||  |          |
              |                                       ||  true       "hi"
              |                                       |"hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s2 endsWith \"hi\"") {
        org.scalatest.diagrams.Diagrams.assert(s2 endsWith "hi")
        org.scalatest.diagrams.Diagrams.assert(s2.endsWith("hi"))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s1 endsWith "hi")
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(s1 endsWith "hi")
              |                                       |  |        |
              |                                       |  false    "hi"
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s1.endsWith("hi"))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(s1.endsWith("hi"))
              |                                       |  |        |
              |                                       |  false    "hi"
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci2 endsWith 1") {
        org.scalatest.diagrams.Diagrams.assert(ci2 endsWith 1)
        org.scalatest.diagrams.Diagrams.assert(ci2.endsWith(1))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ci1 endsWith 1") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1 endsWith 1)
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(ci1 endsWith 1)
              |                                       |   |        |
              |                                       123 false    1
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1.endsWith(1))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(ci1.endsWith(1))
              |                                       |   |        |
              |                                       123 false    1
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
        org.scalatest.diagrams.Diagrams.assert(!s1.endsWith("hi"))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!s2.endsWith("hi"))
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!s2.endsWith("hi"))
              |                                       ||  |        |
              |                                       ||  true     "hi"
              |                                       |"ScalaTest hi"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s3 contains \"hi\"") {
        org.scalatest.diagrams.Diagrams.assert(s3 contains "hi")
        org.scalatest.diagrams.Diagrams.assert(s3.contains("hi"))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s3 contains \"hello\"") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s3 contains "hello")
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(s3 contains "hello")
              |                                       |  |        |
              |                                       |  false    "hello"
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s3.contains("hello"))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(s3.contains("hello"))
              |                                       |  |        |
              |                                       |  false    "hello"
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci2 contains 2") {
        org.scalatest.diagrams.Diagrams.assert(ci2 contains 2)
        org.scalatest.diagrams.Diagrams.assert(ci2.contains(2))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ci1 contains 5") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1 contains 5)
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(ci1 contains 5)
              |                                       |   |        |
              |                                       123 false    5
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1.contains(5))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(ci1.contains(5))
              |                                       |   |        |
              |                                       123 false    5
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s1.contains(\"hello\")") {
        org.scalatest.diagrams.Diagrams.assert(!s3.contains("hello"))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!s3.contains("hi"))
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!s3.contains("hi"))
              |                                       ||  |        |
              |                                       ||  true     "hi"
              |                                       |"Say hi to ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1 contains 2") {
        org.scalatest.diagrams.Diagrams.assert(l1 contains 2)
        org.scalatest.diagrams.Diagrams.assert(l1.contains(2))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1 contains 5") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1 contains 5)
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(l1 contains 5)
              |                                       |  |        |
              |                                       |  false    5
              |                                       List(1, 2, 3)
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.contains(5))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(l1.contains(5))
              |                                       |  |        |
              |                                       |  false    5
              |                                       List(1, 2, 3)
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !(l1 contains 5)") {
        org.scalatest.diagrams.Diagrams.assert(!(l1 contains 5))
        org.scalatest.diagrams.Diagrams.assert(!l1.contains(5))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(l1 contains 2)") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(l1 contains 2))
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!(l1 contains 2))
              |                                       | |  |        |
              |                                       | |  true     2
              |                                       | List(1, 2, 3)
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!l1.contains(2))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!l1.contains(2))
              |                                       ||  |        |
              |                                       ||  true     2
              |                                       |List(1, 2, 3)
              |                                       false
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check m1 contains 2") {
        org.scalatest.diagrams.Diagrams.assert(m1 contains 2)
        org.scalatest.diagrams.Diagrams.assert(m1.contains(2))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check m1 contains 5") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(m1 contains 5)
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(m1 contains 5)
            |                                       |  |        |
            |                                       |  false    5
            |                                       $m1Str
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(m1.contains(5))
        }
        e2.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(m1.contains(5))
            |                                       |  |        |
            |                                       |  false    5
            |                                       $m1Str
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !(m1 contains 5)") {
        org.scalatest.diagrams.Diagrams.assert(!(m1 contains 5))
        org.scalatest.diagrams.Diagrams.assert(!m1.contains(5))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(m1 contains 2)") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(m1 contains 2))
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(!(m1 contains 2))
            |                                       | |  |        |
            |                                       | |  true     2
            |                                       | $m1Str
            |                                       false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!m1.contains(2))
        }
        e2.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(!m1.contains(2))
            |                                       ||  |        |
            |                                       ||  true     2
            |                                       |$m1Str
            |                                       false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ct1 contains 8") {
        org.scalatest.diagrams.Diagrams.assert(ct1 contains 8)
        org.scalatest.diagrams.Diagrams.assert(ct1.contains(8))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ct1 contains 5") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ct1 contains 5)
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(ct1 contains 5)
            |                                       |   |        |
            |                                       |   false    5
            |                                       $ct1Str
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ct1.contains(5))
        }
        e2.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(ct1.contains(5))
            |                                       |   |        |
            |                                       |   false    5
            |                                       $ct1Str
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ct1.contains(5)") {
        org.scalatest.diagrams.Diagrams.assert(!ct1.contains(5))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !ct1.contains(8)") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!ct1.contains(8))
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(!ct1.contains(8))
            |                                       ||   |        |
            |                                       ||   true     8
            |                                       |$ct1Str
            |                                       false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ci1 eq ci3") {
        org.scalatest.diagrams.Diagrams.assert(ci1 eq ci3)
        org.scalatest.diagrams.Diagrams.assert(ci1.eq(ci3))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ci1 eq ci2") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1 eq ci2)
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(ci1 eq ci2)
            |                                       |   |  |
            |                                       $ci1Str |  $ci2Str
            |                                           false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1.eq(ci2))
        }
        e2.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(ci1.eq(ci2))
            |                                       |   |  |
            |                                       $ci1Str |  $ci2Str
            |                                           false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ci1.eq(ci2)") {
        org.scalatest.diagrams.Diagrams.assert(!ci1.eq(ci2))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!ci1.eq(ci3))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(!ci1.eq(ci3))
            |                                       ||   |  |
            |                                       |$ci1Str |  $ci3Str
            |                                       |    true
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ci1 ne ci2") {
        org.scalatest.diagrams.Diagrams.assert(ci1 ne ci2)
        org.scalatest.diagrams.Diagrams.assert(ci1.ne(ci2))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ci1 ne ci3") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1 ne ci3)
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(ci1 ne ci3)
            |                                       |   |  |
            |                                       $ci1Str |  $ci3Str
            |                                           false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1.ne(ci3))
        }
        e2.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(ci1.ne(ci3))
            |                                       |   |  |
            |                                       $ci1Str |  $ci3Str
            |                                           false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ci1.ne(ci3)") {
        org.scalatest.diagrams.Diagrams.assert(!ci1.ne(ci3))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!ci1.ne(ci2))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!ci1.ne(ci2))
              |                                       ||   |  |
              |                                       |123 |  321
              |                                       |    true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s4.isEmpty") {
        org.scalatest.diagrams.Diagrams.assert(s4.isEmpty)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s3.isEmpty") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s3.isEmpty)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(s3.isEmpty)
              |                                       |  |
              |                                       |  false
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !s3.isEmpty") {
        org.scalatest.diagrams.Diagrams.assert(!s3.isEmpty)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !s4.isEmpty") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!s4.isEmpty)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!s4.isEmpty)
              |                                       ||  |
              |                                       |"" true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check l2.isEmpty") {
        org.scalatest.diagrams.Diagrams.assert(l2.isEmpty)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.isEmpty") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.isEmpty)
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.isEmpty)
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.isEmpty") {
        org.scalatest.diagrams.Diagrams.assert(!l1.isEmpty)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !l2.isEmpty") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!l2.isEmpty)
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(!l2.isEmpty)
            |                                       ||  |
            |                                       ||  true
            |                                       |$l2
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s1.isInstanceOf[String]") {
        org.scalatest.diagrams.Diagrams.assert(s1.isInstanceOf[String])
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.isInstanceOf[String])
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.isInstanceOf[String])
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
        org.scalatest.diagrams.Diagrams.assert(l1.isInstanceOf[List[Int]])
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s1.isInstanceOf[List[Int]])
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(s1.isInstanceOf[List[Int]])
              |                                       |  |
              |                                       |  false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check date.isInstanceOf[Date]") {
        org.scalatest.diagrams.Diagrams.assert(date.isInstanceOf[Date])
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.isInstanceOf[Date])
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.isInstanceOf[Date])
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.isInstanceOf[String]") {
        org.scalatest.diagrams.Diagrams.assert(!l1.isInstanceOf[String])
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!s1.isInstanceOf[String])
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!s1.isInstanceOf[String])
              |                                       ||  |
              |                                       ||  true
              |                                       |"hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
        org.scalatest.diagrams.Diagrams.assert(!s1.isInstanceOf[List[Int]])
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!l1.isInstanceOf[List[Int]])
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(!l1.isInstanceOf[List[Int]])
            |                                       ||  |
            |                                       ||  true
            |                                       |$l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
        org.scalatest.diagrams.Diagrams.assert(!l1.isInstanceOf[Date])
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!date.isInstanceOf[Date])
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(!date.isInstanceOf[Date])
            |                                       ||    |
            |                                       ||    true
            |                                       |$date
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s1.length == 9") {
        org.scalatest.diagrams.Diagrams.assert(s1.length == 12)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s1.length == 10") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s1.length == 10)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(s1.length == 10)
              |                                       |  |      |  |
              |                                       |  12     |  10
              |                                       |         false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1.length == 3") {
        org.scalatest.diagrams.Diagrams.assert(l1.length == 3)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.length == 10") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.length == 10)
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.length == 10)
            |                                       |  |      |  |
            |                                       |  3      |  10
            |                                       |         false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(s1.length == 10)") {
        org.scalatest.diagrams.Diagrams.assert(!(s1.length == 10))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.length == 9)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(s1.length == 12))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!(s1.length == 12))
              |                                       | |  |      |  |
              |                                       | |  12     |  12
              |                                       | |         true
              |                                       | "hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check !(l1.length == 2)") {
        org.scalatest.diagrams.Diagrams.assert(!(l1.length == 2))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.length == 9)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(l1.length == 3))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(!(l1.length == 3))
            |                                       | |  |      |  |
            |                                       | |  3      |  3
            |                                       | |         true
            |                                       | $l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check s1.size == 9") {
        org.scalatest.diagrams.Diagrams.assert(s1.size == 12)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s1.size == 10") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s1.size == 10)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(s1.size == 10)
              |                                       |  |    |  |
              |                                       |  12   |  10
              |                                       |       false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1.size == 3") {
        org.scalatest.diagrams.Diagrams.assert(l1.size == 3)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.size == 10") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.size == 10)
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.size == 10)
            |                                       |  |    |  |
            |                                       |  3    |  10
            |                                       |       false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(s1.size == 10)") {
        org.scalatest.diagrams.Diagrams.assert(!(s1.size == 10))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.size == 9)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(s1.size == 12))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(!(s1.size == 12))
              |                                       | |  |    |  |
              |                                       | |  12   |  12
              |                                       | |       true
              |                                       | "hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check !(l1.size == 2)") {
        org.scalatest.diagrams.Diagrams.assert(!(l1.size == 2))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.size == 9) ") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(l1.size == 3))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(!(l1.size == 3))
            |                                       | |  |    |  |
            |                                       | |  3    |  3
            |                                       | |       true
            |                                       | $l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check l1.exists(_ == 3)") {
        org.scalatest.diagrams.Diagrams.assert(l1.exists(_ == 3))
      }

      it("should do nothing when is used to check l1.exists(3 == _)") {
        org.scalatest.diagrams.Diagrams.assert(l1.exists(3 == _))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ == 5) ") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.exists(_ == 5))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.exists(_ == 5))
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(5 == _) ") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.exists(5 == _))
        }
        e.message should be (
          Some(
            s"""
               |
               |org.scalatest.diagrams.Diagrams.assert(l1.exists(5 == _))
               |                                       |  |
               |                                       |  false
               |                                       $l1
                |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.exists(_ == 5)") {
        org.scalatest.diagrams.Diagrams.assert(!l1.exists(_ == 5))
      }

      it("should do nothing when is used to check !l1.exists(5 == _)") {
        org.scalatest.diagrams.Diagrams.assert(!l1.exists(5 == _))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!l1.exists(_ == 3))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(!l1.exists(_ == 3))
            |                                       ||  |
            |                                       ||  true
            |                                       |$l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!l1.exists(3 == _))
        }
        e.message should be (
          Some(
            s"""
               |
               |org.scalatest.diagrams.Diagrams.assert(!l1.exists(3 == _))
               |                                       ||  |
               |                                       ||  true
               |                                       |$l1
               |                                       false
               |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.exists(_ > 3))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.exists(_ > 3))
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(3 < _)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.exists(3 < _))
        }
        e.message should be (
          Some(
            s"""
               |
               |org.scalatest.diagrams.Diagrams.assert(l1.exists(3 < _))
               |                                       |  |
               |                                       |  false
               |                                       $l1
                |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l3.exists(_.isEmpty))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assert(l3.exists(_.isEmpty))
            |                                       |  |
            |                                       |  false
            |                                       $l3Str
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(false)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1.exists(321))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(ci1.exists(321))
              |                                       |   |      |
              |                                       123 false  321
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when used to check woof { meow(y = 5) } == \"woof\" ") {
        org.scalatest.diagrams.Diagrams.assert(woof { meow(y = 5) } == "woof")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(woof { meow(y = 5) } == "meow")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(woof { meow(y = 5) } == "meow")
              |                                       |                    |  |
              |                                       "woof"               |  "meow"
              |                                                            false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when used to check multiline org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 2 <= a)) ") {
        org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 2 <=
          a))
      }

      // SKIP-DOTTY-START
      // problem with quotes
      it("should throw friend message when used to check multiline org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 1 <= a))") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 1 <=
            a))
        }
        e.message shouldBe Some("5 equaled 5, but 4 was not less than or equal to 3")
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
      }
      // SKIP-DOTTY-END

      it("should do nothing when a block of code that evaluates to true is passed in") {
        assert {
          val a = 1
          val b = 2
          val c = a + b
          a < b || c == a + b
        }
      }

      it("should throw TestFailedException with correct message and stack depth when a block of code that evaluates to false is passed") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert { val a = 1; val b = 2; val c = a + b; a > b || c == b * b }
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert { val a = 1; val b = 2; val c = a + b; a > b || c == b * b }
              |                                                                              | | | |  | |  | | |
              |                                                                              1 | 2 |  3 |  2 4 2
              |                                                                                |   |    false
              |                                                                                |   false
              |                                                                                false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      // SKIP-DOTTY-START
      // different code show in Dotty
      it("should fallback to BooleanMacro when a block of code > 1 line is passed in ") {
        val e = intercept[TestFailedException] {
          assert {
            val a = 1
            val b = 2
            val c = a + b
            a > b || c == b * b }
        }
        e.message should be (
          Some(
            """{
              |  val a: Int = 1;
              |  val b: Int = 2;
              |  val c: Int = a.+(b);
              |  a.>(b).||(c.==(b.*(b)))
              |} was false""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 17))
      }
      // SKIP-DOTTY-END

      // SKIP-SCALATESTJS,NATIVE-START
      it("should do nothing when used to check <person>Dude</person> == <person>Dude</person>") {
        org.scalatest.diagrams.Diagrams.assert(<person>Dude</person> == <person>Dude</person>)
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check <person>Dude</person> == <person>Mary</person>") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(<person>Dude</person> == <person>Mary</person>)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(<person>Dude</person> == <person>Mary</person>)
              |                                       |                     |  |
              |                                       <person>Dude</person> |  <person>Mary</person>
              |                                                             false
              |""".stripMargin
          )
        )
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should compile when used with org == xxx that shadow org.scalactic ") {
        assertCompiles(
          """
            |val org = "test"
            |_root_.org.scalatest.diagrams.Diagrams.assert(org == "test")
          """.stripMargin)
      }

      it("should compile when used with org === xxx that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = "test"
            |_root_.org.scalatest.diagrams.Diagrams.assert(org === "test")
          """.stripMargin)
      }

      it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
        assertCompiles(
          """
            |class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
            |  it("testing here") {
            |    val org = "test"
            |    _root_.org.scalatest.diagrams.Diagrams.assert(org === "test")
            |  }
            |}
          """.stripMargin)
      }

      it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
        assertCompiles(
          """
            |class Test {
            |  def aCustomMethod: Boolean = true
            |}
            |val org = new Test
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.aCustomMethod)
          """.stripMargin)
      }

      it("should compile when used with !org that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = false
            |_root_.org.scalatest.diagrams.Diagrams.assert(!org)
          """.stripMargin)
      }

      it("should compile when used with org.isEmpty that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.isEmpty)
          """.stripMargin)
      }

      it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.isInstanceOf[String])
          """.stripMargin)
      }

      it("should compile when used with org.size == 0 that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = Array.empty[String]
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.size == 0)
          """.stripMargin)
      }

      it("should compile when used with org.length == 0 that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.length == 0)
          """.stripMargin)
      }

      it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
        assertCompiles(
          """
            |val org = "abc"
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.exists(_ == 'b'))
          """.stripMargin)
      }

      it("should do nothing when is used to check new String(\"test\") != \"test\"") {
        org.scalatest.diagrams.Diagrams.assert(new String("test") == "test")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check new String(\"test\") != \"testing\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(new String("test") == "testing")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assert(new String("test") == "testing")
              |                                       |                  |  |
              |                                       "test"             |  "testing"
              |                                                          false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should compile when used with Java static method") {
        assertCompiles(
          """
            |org.scalatest.diagrams.Diagrams.assert(System.currentTimeMillis() > 0)
          """.stripMargin)
        assertCompiles(
          """
            |org.scalatest.diagrams.Diagrams.assert(java.math.BigInteger.ZERO == java.math.BigInteger.ZERO)
          """.stripMargin)
      }
    }

    describe("The org.scalatest.diagrams.Diagrams.assert(boolean, clue) method") {
      it("should do nothing when is used to check a == 3") {
        org.scalatest.diagrams.Diagrams.assert(a == 3, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 5") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 5, "this is a clue")
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 == b") {
        org.scalatest.diagrams.Diagrams.assert(5 == b, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 3 == b") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(3 == b, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(3 == b, "this is a clue")
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a != 5") {
        org.scalatest.diagrams.Diagrams.assert(a != 5, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a != 3") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a != 3, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a != 3, "this is a clue")
              |                                       | |  |
              |                                       3 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 != b") {
        org.scalatest.diagrams.Diagrams.assert(3 != b, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 5 != b") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(5 != b, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(5 != b, "this is a clue")
              |                                       | |  |
              |                                       5 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 == 3") {
        org.scalatest.diagrams.Diagrams.assert(3 == 3, "this is a clue")
      }

      // SKIP-DOTTY-START
      // const-folding will eliminate the expr
      it("should throw TestFailedException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
        // This is because the compiler simply pass the false boolean literal
        // to the macro, can't find a way to get the 3 == 5 literal.
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(3 == 5, "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(3 == 5, "this is a clue")
              |                                         |
              |                                         false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }
      // SKIP-DOTTY-END

      it("should throw TestFailedException with correct message and stack depth when is used to check a == b") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == b, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == b, "this is a clue")
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      // SKIP-DOTTY-START
      it("should throw TestFailedException with correct message and stack depth when is used to check a == null") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == null, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == null, "this is a clue")
              |                                       | |  |
              |                                       3 |  null
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(null == a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(null == a, "this is a clue")
              |                                       |    |  |
              |                                       null |  3
              |                                            false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }
      // SKIP-DOTTY-END

      it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(3 != a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(3 != a, "this is a clue")
              |                                       | |  |
              |                                       3 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 != a") {
        org.scalatest.diagrams.Diagrams.assert(5 != a, "this is a clue")
      }

      it("should do nothing when is used to check a > 2") {
        org.scalatest.diagrams.Diagrams.assert(a > 2, "this is a clue")
      }

      it("should do nothing when is used to check 5 > a") {
        org.scalatest.diagrams.Diagrams.assert(5 > a, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a > 3") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a > 3, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a > 3, "this is a clue")
              |                                       | | |
              |                                       3 | 3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 3 > a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(3 > a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(3 > a, "this is a clue")
              |                                       | | |
              |                                       3 | 3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a >= 3") {
        org.scalatest.diagrams.Diagrams.assert(a >= 3, "this is a clue")
      }

      it("should do nothing when is used to check 3 >= a") {
        org.scalatest.diagrams.Diagrams.assert(3 >= a, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a >= 4") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a >= 4, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a >= 4, "this is a clue")
              |                                       | |  |
              |                                       3 |  4
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 2 >= a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(2 >= a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(2 >= a, "this is a clue")
              |                                       | |  |
              |                                       2 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check b < 6") {
        org.scalatest.diagrams.Diagrams.assert(b < 6, "this is a clue")
      }

      it("should do nothing when is used to check 3 < b") {
        org.scalatest.diagrams.Diagrams.assert(3 < b, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check b < 5") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(b < 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(b < 5, "this is a clue")
              |                                       | | |
              |                                       5 | 5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 5 < b") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(5 < b, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(5 < b, "this is a clue")
              |                                       | | |
              |                                       5 | 5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check b <= 5") {
        org.scalatest.diagrams.Diagrams.assert(b <= 5, "this is a clue")
      }

      it("should do nothing when is used to check 5 <= b") {
        org.scalatest.diagrams.Diagrams.assert(5 <= b, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check b <= 4") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(b <= 4, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(b <= 4, "this is a clue")
              |                                       | |  |
              |                                       5 |  4
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 6 <= b") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(6 <= b, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(6 <= b, "this is a clue")
              |                                       | |  |
              |                                       6 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check bob == \"bob\"") {
        org.scalatest.diagrams.Diagrams.assert(bob == "bob", "this is a clue")
      }

      it("should do nothing when is used to check bob != \"alice\"") {
        org.scalatest.diagrams.Diagrams.assert(bob != "alice", "this is a clue")
      }

      it("should do nothing when is used to check alice == \"alice\"") {
        org.scalatest.diagrams.Diagrams.assert(alice == "alice", "this is a clue")
      }

      it("should do nothing when is used to check alice != \"bob\"") {
        org.scalatest.diagrams.Diagrams.assert(alice != "bob", "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check bob == \"alice\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(bob == "alice", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(bob == "alice", "this is a clue")
              |                                       |   |  |
              |                                       |   |  "alice"
              |                                       |   false
              |                                       "bob"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check bob != \"bob\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(bob != "bob", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(bob != "bob", "this is a clue")
              |                                       |   |  |
              |                                       |   |  "bob"
              |                                       |   false
              |                                       "bob"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check alice == \"bob\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(alice == "bob", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(alice == "bob", "this is a clue")
              |                                       |     |  |
              |                                       |     |  "bob"
              |                                       |     false
              |                                       "alice"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check alice != \"alice\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(alice != "alice", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(alice != "alice", "this is a clue")
              |                                       |     |  |
              |                                       |     |  "alice"
              |                                       |     false
              |                                       "alice"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check a === 3") {
        org.scalatest.diagrams.Diagrams.assert(a === 3, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a === 5 ") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a === 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a === 5, "this is a clue")
              |                                       | |   |
              |                                       3 |   5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 === a") {
        org.scalatest.diagrams.Diagrams.assert(3 === a, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(5 === a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(5 === a, "this is a clue")
              |                                       | |   |
              |                                       5 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a !== 5") {
        org.scalatest.diagrams.Diagrams.assert(a !== 5, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a !== 3") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a !== 3, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a !== 3, "this is a clue")
              |                                       | |   |
              |                                       3 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 !== a") {
        org.scalatest.diagrams.Diagrams.assert(5 !== a, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check 3 !== a") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(3 !== a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(3 !== a, "this is a clue")
              |                                       | |   |
              |                                       3 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 && b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 && b == 5, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 3 && b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 3 && b == 6, "this is a clue")
              |                                       | |  | |  | |  |
              |                                       3 |  3 |  5 |  6
              |                                         true |    false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 5") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 && b == 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 && b == 5, "this is a clue")
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 && b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 && b == 6, "this is a clue")
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 & b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 & b == 5, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 & b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 3 & b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 3 & b == 6, "this is a clue")
              |                                       | |  | | | |  |
              |                                       3 |  3 | 5 |  6
              |                                         true |   false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 5") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 & b == 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 & b == 5, "this is a clue")
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 & b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 & b == 6, "this is a clue")
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 || b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 || b == 5, "this is a clue")
      }

      it("should do nothing when is used to check a == 3 || b == 6") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 || b == 6, "this is a clue")
      }

      it("should do nothing when is used to check a == 2 || b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 2 || b == 5, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 || b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 || b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 || b == 6, "this is a clue")
              |                                       | |  | |  | |  |
              |                                       3 |  2 |  5 |  6
              |                                         |    |    false
              |                                         |    false
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check a == 3 | b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 | b == 5, "this is a clue")
      }

      it("should do nothing when is used to check a == 3 | b == 6") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 | b == 6, "this is a clue")
      }

      it("should do nothing when is used to check a == 2 | b == 5") {
        org.scalatest.diagrams.Diagrams.assert(a == 2 | b == 5, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 | b == 6") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 2 | b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 2 | b == 6, "this is a clue")
              |                                       | |  | | | |  |
              |                                       3 |  2 | 5 |  6
              |                                         |    |   false
              |                                         |    false
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 && (b == 5 && b > 3), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 3 && (b == 5 && b > 5), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 3 && (b == 5 && b > 5), "this is a clue")
              |                                       | |  | |   | |  | |  | | |
              |                                       3 |  3 |   5 |  5 |  5 | 5
              |                                         true false true |    false
              |                                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(a == 5)") {
        org.scalatest.diagrams.Diagrams.assert(!(a == 5), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(a == 3)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(a == 3), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!(a == 3), "this is a clue")
              |                                       | | |  |
              |                                       | 3 |  3
              |                                       |   true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 3 && !(b == 5), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 3 && !(b == 5), "this is a clue")
              |                                       | |  | |  | | |  |
              |                                       3 |  3 |  | 5 |  5
              |                                         true |  |   true
              |                                              |  false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check (a == 3) == (b == 5)") {
        org.scalatest.diagrams.Diagrams.assert((a == 3) == (b == 5), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert((a == 3) == (b != 5), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert((a == 3) == (b != 5), "this is a clue")
              |                                        | |  |  |   | |  |
              |                                        3 |  3  |   5 |  5
              |                                          true  false false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should short-circuit && when first condition was false") {
        val s = new Stateful
        intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 5 && s.changeState, "this is a clue")
        }
        s.state should be (false)
      }

      it("should short-circuit & when first condition was false") {
        val s = new Stateful
        intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 5 & s.changeState, "this is a clue")
        }
        s.state should be (false)
      }

      it("should short-circuit || when first condition was true") {
        val s = new Stateful
        org.scalatest.diagrams.Diagrams.assert(a == 3 || s.changeState, "this is a clue")
        s.state should be (false)
      }

      it("should short-circuit | when first condition was true") {
        val s = new Stateful
        org.scalatest.diagrams.Diagrams.assert(a == 3 | s.changeState, "this is a clue")
        s.state should be (false)
      }

      it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5} ") {
        org.scalatest.diagrams.Diagrams.assert(a == 3 && { println("hi"); b == 5}, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(a == 3 && { println("hi"); b == 3}, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(a == 3 && { println("hi"); b == 3}, "this is a clue")
              |                                       | |  | |                   | |  |
              |                                       3 |  3 false               5 |  3
              |                                         true                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
        org.scalatest.diagrams.Diagrams.assert({ println("hi"); b == 5} && a == 3, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert({ println("hi"); b == 5} && a == 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert({ println("hi"); b == 5} && a == 5, "this is a clue")
              |                                                        | |  |  |  | |  |
              |                                                        5 |  5  |  3 |  5
              |                                                          true  |    false
              |                                                                false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should preserve side effects when Apply with single argument is passed in") {
        org.scalatest.diagrams.Diagrams.assert(neverRuns1(sys.error("Sad times 1")), "this is a clue")
      }

      it("should preserve side effects when Apply with 2 argument list is passed in") {
        org.scalatest.diagrams.Diagrams.assert(neverRuns2(sys.error("Sad times 2"))(0), "this is a clue")
      }

      it("should preserve side effects when typed Apply with 2 argument list is passed in") {
        org.scalatest.diagrams.Diagrams.assert(neverRuns3(sys.error("Sad times 3"))(0), "this is a clue")
      }

      it("should do nothing when is used to check s1 startsWith \"hi\"") {
        org.scalatest.diagrams.Diagrams.assert(s1 startsWith "hi", "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(s1.startsWith("hi"), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s2 startsWith "hi", "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(s2 startsWith "hi", "this is a clue")
              |                                       |  |          |
              |                                       |  false      "hi"
              |                                       "ScalaTest hi"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s2.startsWith("hi"), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(s2.startsWith("hi"), "this is a clue")
              |                                       |  |          |
              |                                       |  false      "hi"
              |                                       "ScalaTest hi"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci1 startsWith 1") {
        org.scalatest.diagrams.Diagrams.assert(ci1 startsWith 1, "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(ci1.startsWith(1), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ci2 startsWith 1") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci2 startsWith 1, "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(ci2 startsWith 1, "this is a clue")
              |                                       |   |          |
              |                                       321 false      1
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci2.startsWith(1), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(ci2.startsWith(1), "this is a clue")
              |                                       |   |          |
              |                                       321 false      1
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
        org.scalatest.diagrams.Diagrams.assert(!s2.startsWith("hi"), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!s1.startsWith("hi"), "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!s1.startsWith("hi"), "this is a clue")
              |                                       ||  |          |
              |                                       ||  true       "hi"
              |                                       |"hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s2 endsWith \"hi\"") {
        org.scalatest.diagrams.Diagrams.assert(s2 endsWith "hi", "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(s2.endsWith("hi"), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s1 endsWith "hi", "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(s1 endsWith "hi", "this is a clue")
              |                                       |  |        |
              |                                       |  false    "hi"
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s1.endsWith("hi"), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(s1.endsWith("hi"), "this is a clue")
              |                                       |  |        |
              |                                       |  false    "hi"
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci2 endsWith 1") {
        org.scalatest.diagrams.Diagrams.assert(ci2 endsWith 1, "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(ci2.endsWith(1), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ci1 endsWith 1") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1 endsWith 1, "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(ci1 endsWith 1, "this is a clue")
              |                                       |   |        |
              |                                       123 false    1
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1.endsWith(1), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(ci1.endsWith(1), "this is a clue")
              |                                       |   |        |
              |                                       123 false    1
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
        org.scalatest.diagrams.Diagrams.assert(!s1.endsWith("hi"), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!s2.endsWith("hi"), "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!s2.endsWith("hi"), "this is a clue")
              |                                       ||  |        |
              |                                       ||  true     "hi"
              |                                       |"ScalaTest hi"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s3 contains \"hi\"") {
        org.scalatest.diagrams.Diagrams.assert(s3 contains "hi", "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(s3.contains("hi"), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s3 contains \"hello\"") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s3 contains "hello", "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(s3 contains "hello", "this is a clue")
              |                                       |  |        |
              |                                       |  false    "hello"
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s3.contains("hello"), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(s3.contains("hello"), "this is a clue")
              |                                       |  |        |
              |                                       |  false    "hello"
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci2 contains 2") {
        org.scalatest.diagrams.Diagrams.assert(ci2 contains 2, "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(ci2.contains(2), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ci1 contains 5") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1 contains 5, "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(ci1 contains 5, "this is a clue")
              |                                       |   |        |
              |                                       123 false    5
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1.contains(5), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(ci1.contains(5), "this is a clue")
              |                                       |   |        |
              |                                       123 false    5
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s1.contains(\"hello\")") {
        org.scalatest.diagrams.Diagrams.assert(!s3.contains("hello"), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!s3.contains("hi"), "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!s3.contains("hi"), "this is a clue")
              |                                       ||  |        |
              |                                       ||  true     "hi"
              |                                       |"Say hi to ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1 contains 2") {
        org.scalatest.diagrams.Diagrams.assert(l1 contains 2, "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(l1.contains(2), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1 contains 5") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1 contains 5, "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(l1 contains 5, "this is a clue")
              |                                       |  |        |
              |                                       |  false    5
              |                                       List(1, 2, 3)
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.contains(5), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(l1.contains(5), "this is a clue")
              |                                       |  |        |
              |                                       |  false    5
              |                                       List(1, 2, 3)
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !(l1 contains 5)") {
        org.scalatest.diagrams.Diagrams.assert(!(l1 contains 5), "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(!l1.contains(5), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(l1 contains 2)") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(l1 contains 2), "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!(l1 contains 2), "this is a clue")
              |                                       | |  |        |
              |                                       | |  true     2
              |                                       | List(1, 2, 3)
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!l1.contains(2), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!l1.contains(2), "this is a clue")
              |                                       ||  |        |
              |                                       ||  true     2
              |                                       |List(1, 2, 3)
              |                                       false
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check m1 contains 2") {
        org.scalatest.diagrams.Diagrams.assert(m1 contains 2, "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(m1.contains(2), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check m1 contains 5") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(m1 contains 5, "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(m1 contains 5, "this is a clue")
            |                                       |  |        |
            |                                       |  false    5
            |                                       $m1Str
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(m1.contains(5), "this is a clue")
        }
        e2.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(m1.contains(5), "this is a clue")
            |                                       |  |        |
            |                                       |  false    5
            |                                       $m1Str
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !(m1 contains 5)") {
        org.scalatest.diagrams.Diagrams.assert(!(m1 contains 5), "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(!m1.contains(5), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(m1 contains 2)") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(m1 contains 2), "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(!(m1 contains 2), "this is a clue")
            |                                       | |  |        |
            |                                       | |  true     2
            |                                       | $m1Str
            |                                       false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!m1.contains(2), "this is a clue")
        }
        e2.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(!m1.contains(2), "this is a clue")
            |                                       ||  |        |
            |                                       ||  true     2
            |                                       |$m1Str
            |                                       false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ct1 contains 8") {
        org.scalatest.diagrams.Diagrams.assert(ct1 contains 8, "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(ct1.contains(8), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ct1 contains 5") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ct1 contains 5, "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(ct1 contains 5, "this is a clue")
            |                                       |   |        |
            |                                       |   false    5
            |                                       $ct1Str
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ct1.contains(5), "this is a clue")
        }
        e2.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(ct1.contains(5), "this is a clue")
            |                                       |   |        |
            |                                       |   false    5
            |                                       $ct1Str
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ct1.contains(5)") {
        org.scalatest.diagrams.Diagrams.assert(!ct1.contains(5), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !ct1.contains(8)") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!ct1.contains(8), "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(!ct1.contains(8), "this is a clue")
            |                                       ||   |        |
            |                                       ||   true     8
            |                                       |$ct1Str
            |                                       false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ci1 eq ci3") {
        org.scalatest.diagrams.Diagrams.assert(ci1 eq ci3, "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(ci1.eq(ci3), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ci1 eq ci2") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1 eq ci2, "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(ci1 eq ci2, "this is a clue")
            |                                       |   |  |
            |                                       $ci1Str |  $ci2Str
            |                                           false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1.eq(ci2), "this is a clue")
        }
        e2.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(ci1.eq(ci2), "this is a clue")
            |                                       |   |  |
            |                                       $ci1Str |  $ci2Str
            |                                           false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ci1.eq(ci2)") {
        org.scalatest.diagrams.Diagrams.assert(!ci1.eq(ci2), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!ci1.eq(ci3), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(!ci1.eq(ci3), "this is a clue")
            |                                       ||   |  |
            |                                       |$ci1Str |  $ci3Str
            |                                       |    true
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ci1 ne ci2") {
        org.scalatest.diagrams.Diagrams.assert(ci1 ne ci2, "this is a clue")
        org.scalatest.diagrams.Diagrams.assert(ci1.ne(ci2), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check ci1 ne ci3") {
        val e1 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1 ne ci3, "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(ci1 ne ci3, "this is a clue")
            |                                       |   |  |
            |                                       $ci1Str |  $ci3Str
            |                                           false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1.ne(ci3), "this is a clue")
        }
        e2.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(ci1.ne(ci3), "this is a clue")
            |                                       |   |  |
            |                                       $ci1Str |  $ci3Str
            |                                           false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ci1.ne(ci3)") {
        org.scalatest.diagrams.Diagrams.assert(!ci1.ne(ci3), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!ci1.ne(ci2), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!ci1.ne(ci2), "this is a clue")
              |                                       ||   |  |
              |                                       |123 |  321
              |                                       |    true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s4.isEmpty") {
        org.scalatest.diagrams.Diagrams.assert(s4.isEmpty, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s3.isEmpty") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s3.isEmpty, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(s3.isEmpty, "this is a clue")
              |                                       |  |
              |                                       |  false
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !s3.isEmpty") {
        org.scalatest.diagrams.Diagrams.assert(!s3.isEmpty, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !s4.isEmpty") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!s4.isEmpty, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!s4.isEmpty, "this is a clue")
              |                                       ||  |
              |                                       |"" true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check l2.isEmpty") {
        org.scalatest.diagrams.Diagrams.assert(l2.isEmpty, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.isEmpty") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.isEmpty, "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.isEmpty, "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.isEmpty") {
        org.scalatest.diagrams.Diagrams.assert(!l1.isEmpty, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !l2.isEmpty") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!l2.isEmpty, "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(!l2.isEmpty, "this is a clue")
            |                                       ||  |
            |                                       ||  true
            |                                       |$l2
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s1.isInstanceOf[String]") {
        org.scalatest.diagrams.Diagrams.assert(s1.isInstanceOf[String], "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.isInstanceOf[String], "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.isInstanceOf[String], "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
        org.scalatest.diagrams.Diagrams.assert(l1.isInstanceOf[List[Int]], "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s1.isInstanceOf[List[Int]], "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(s1.isInstanceOf[List[Int]], "this is a clue")
              |                                       |  |
              |                                       |  false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check date.isInstanceOf[Date]") {
        org.scalatest.diagrams.Diagrams.assert(date.isInstanceOf[Date], "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.isInstanceOf[Date], "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.isInstanceOf[Date], "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.isInstanceOf[String]") {
        org.scalatest.diagrams.Diagrams.assert(!l1.isInstanceOf[String], "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!s1.isInstanceOf[String], "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!s1.isInstanceOf[String], "this is a clue")
              |                                       ||  |
              |                                       ||  true
              |                                       |"hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
        org.scalatest.diagrams.Diagrams.assert(!s1.isInstanceOf[List[Int]], "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!l1.isInstanceOf[List[Int]], "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(!l1.isInstanceOf[List[Int]], "this is a clue")
            |                                       ||  |
            |                                       ||  true
            |                                       |$l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
        org.scalatest.diagrams.Diagrams.assert(!l1.isInstanceOf[Date], "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!date.isInstanceOf[Date], "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(!date.isInstanceOf[Date], "this is a clue")
            |                                       ||    |
            |                                       ||    true
            |                                       |$date
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s1.length == 9") {
        org.scalatest.diagrams.Diagrams.assert(s1.length == 12, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s1.length == 10") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s1.length == 10, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(s1.length == 10, "this is a clue")
              |                                       |  |      |  |
              |                                       |  12     |  10
              |                                       |         false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1.length == 3") {
        org.scalatest.diagrams.Diagrams.assert(l1.length == 3, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.length == 10") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.length == 10, "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.length == 10, "this is a clue")
            |                                       |  |      |  |
            |                                       |  3      |  10
            |                                       |         false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(s1.length == 10)") {
        org.scalatest.diagrams.Diagrams.assert(!(s1.length == 10), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.length == 9)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(s1.length == 12), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!(s1.length == 12), "this is a clue")
              |                                       | |  |      |  |
              |                                       | |  12     |  12
              |                                       | |         true
              |                                       | "hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check !(l1.length == 2)") {
        org.scalatest.diagrams.Diagrams.assert(!(l1.length == 2), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.length == 9)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(l1.length == 3), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(!(l1.length == 3), "this is a clue")
            |                                       | |  |      |  |
            |                                       | |  3      |  3
            |                                       | |         true
            |                                       | $l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check s1.size == 9") {
        org.scalatest.diagrams.Diagrams.assert(s1.size == 12, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check s1.size == 10") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(s1.size == 10, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(s1.size == 10, "this is a clue")
              |                                       |  |    |  |
              |                                       |  12   |  10
              |                                       |       false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1.size == 3") {
        org.scalatest.diagrams.Diagrams.assert(l1.size == 3, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.size == 10") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.size == 10, "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.size == 10, "this is a clue")
            |                                       |  |    |  |
            |                                       |  3    |  10
            |                                       |       false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(s1.size == 10)") {
        org.scalatest.diagrams.Diagrams.assert(!(s1.size == 10), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.size == 9)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(s1.size == 12), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(!(s1.size == 12), "this is a clue")
              |                                       | |  |    |  |
              |                                       | |  12   |  12
              |                                       | |       true
              |                                       | "hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check !(l1.size == 2)") {
        org.scalatest.diagrams.Diagrams.assert(!(l1.size == 2), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.size == 9) ") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!(l1.size == 3), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(!(l1.size == 3), "this is a clue")
            |                                       | |  |    |  |
            |                                       | |  3    |  3
            |                                       | |       true
            |                                       | $l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check l1.exists(_ == 3)") {
        org.scalatest.diagrams.Diagrams.assert(l1.exists(_ == 3), "this is a clue")
      }

      it("should do nothing when is used to check l1.exists(3 == _)") {
        org.scalatest.diagrams.Diagrams.assert(l1.exists(3 == _), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ == 5) ") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.exists(_ == 5), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.exists(_ == 5), "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(5 == _) ") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.exists(5 == _), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
               |
               |org.scalatest.diagrams.Diagrams.assert(l1.exists(5 == _), "this is a clue")
               |                                       |  |
               |                                       |  false
               |                                       $l1
               |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.exists(_ == 5)") {
        org.scalatest.diagrams.Diagrams.assert(!l1.exists(_ == 5), "this is a clue")
      }

      it("should do nothing when is used to check !l1.exists(5 == _)") {
        org.scalatest.diagrams.Diagrams.assert(!l1.exists(5 == _), "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!l1.exists(_ == 3), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(!l1.exists(_ == 3), "this is a clue")
            |                                       ||  |
            |                                       ||  true
            |                                       |$l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(!l1.exists(3 == _), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
               |
               |org.scalatest.diagrams.Diagrams.assert(!l1.exists(3 == _), "this is a clue")
               |                                       ||  |
               |                                       ||  true
               |                                       |$l1
               |                                       false
               |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.exists(_ > 3), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(l1.exists(_ > 3), "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(3 < _)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l1.exists(3 < _), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
               |
               |org.scalatest.diagrams.Diagrams.assert(l1.exists(3 < _), "this is a clue")
               |                                       |  |
               |                                       |  false
               |                                       $l1
               |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(l3.exists(_.isEmpty), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assert(l3.exists(_.isEmpty), "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l3Str
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(false)") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(ci1.exists(321), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(ci1.exists(321), "this is a clue")
              |                                       |   |      |
              |                                       123 false  321
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
        org.scalatest.diagrams.Diagrams.assert(woof { meow(y = 5) } == "woof", "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(woof { meow(y = 5) } == "meow", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(woof { meow(y = 5) } == "meow", "this is a clue")
              |                                       |                    |  |
              |                                       "woof"               |  "meow"
              |                                                            false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when used to check multiline org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 2 <= a)) ") {
        org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 2 <=
          a), "this is a clue")
      }

      // SKIP-DOTTY-START
      // problem with quotes
      it("should throw friend message when used to check multiline org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 1 <= a))") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 1 <=
            a), "this is a clue")
        }
        e.message shouldBe Some("5 equaled 5, but 4 was not less than or equal to 3 this is a clue")
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
      }
      // SKIP-DOTTY-END

      it("should do nothing when a block of code that evaluates to true is passed in") {
        org.scalatest.diagrams.Diagrams.assert({
          val a = 1
          val b = 2
          val c = a + b
          a < b || c == a + b
        }, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when a block of code that evaluates to false is passed") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert({ val a = 1; val b = 2; val c = a + b; a > b || c == b * b }, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert({ val a = 1; val b = 2; val c = a + b; a > b || c == b * b }, "this is a clue")
              |                                                                              | | | |  | |  | | |
              |                                                                              1 | 2 |  3 |  2 4 2
              |                                                                                |   |    false
              |                                                                                |   false
              |                                                                                false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      // SKIP-DOTTY-START
      // different code show in Dotty
      it("should fallback to BooleanMacro when a block of code > 1 line is passed in ") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert({
            val a = 1
            val b = 2
            val c = a + b
            a > b || c == b * b }, "this is a clue")
        }
        e.message should be (
          Some(
            """{
              |  val a: Int = 1;
              |  val b: Int = 2;
              |  val c: Int = a.+(b);
              |  a.>(b).||(c.==(b.*(b)))
              |} was false this is a clue""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 17))
      }
      // SKIP-DOTTY-END

      // SKIP-SCALATESTJS,NATIVE-START
      it("should do nothing when used to check <person>Dude</person> == <person>Dude</person>") {
        org.scalatest.diagrams.Diagrams.assert(<person>Dude</person> == <person>Dude</person>, "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check <person>Dude</person> == <person>Mary</person>") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(<person>Dude</person> == <person>Mary</person>, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(<person>Dude</person> == <person>Mary</person>, "this is a clue")
              |                                       |                     |  |
              |                                       <person>Dude</person> |  <person>Mary</person>
              |                                                             false
              |""".stripMargin
          )
        )
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should compile when used with org == xxx that shadow org.scalactic ") {
        assertCompiles(
          """
            |val org = "test"
            |_root_.org.scalatest.diagrams.Diagrams.assert(org == "test", "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org === xxx that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = "test"
            |_root_.org.scalatest.diagrams.Diagrams.assert(org === "test", "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
        assertCompiles(
          """
            |class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
            |  it("testing here") {
            |    val org = "test"
            |    _root_.org.scalatest.diagrams.Diagrams.assert(org === "test", "this is a clue")
            |  }
            |}
          """.stripMargin)
      }

      it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
        assertCompiles(
          """
            |class Test {
            |  def aCustomMethod: Boolean = true
            |}
            |val org = new Test
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.aCustomMethod, "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with !org that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = false
            |_root_.org.scalatest.diagrams.Diagrams.assert(!org, "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org.isEmpty that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.isEmpty, "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.isInstanceOf[String], "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org.size == 0 that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = Array.empty[String]
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.size == 0, "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org.length == 0 that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.length == 0, "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
        assertCompiles(
          """
            |val org = "abc"
            |_root_.org.scalatest.diagrams.Diagrams.assert(org.exists(_ == 'b'), "this is a clue")
          """.stripMargin)
      }

      it("should do nothing when is used to check new String(\"test\") != \"test\"") {
        org.scalatest.diagrams.Diagrams.assert(new String("test") == "test", "this is a clue")
      }

      it("should throw TestFailedException with correct message and stack depth when is used to check new String(\"test\") != \"testing\"") {
        val e = intercept[TestFailedException] {
          org.scalatest.diagrams.Diagrams.assert(new String("test") == "testing", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assert(new String("test") == "testing", "this is a clue")
              |                                       |                  |  |
              |                                       "test"             |  "testing"
              |                                                          false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should compile when used with Java static method") {
        assertCompiles(
          """
            |org.scalatest.diagrams.Diagrams.assert(System.currentTimeMillis() > 0, "this is a clue")
          """.stripMargin)
        assertCompiles(
          """
            |org.scalatest.diagrams.Diagrams.assert(java.math.BigDecimal.ZERO == java.math.BigDecimal.ZERO, "this is a clue")
          """.stripMargin)
      }
    }

    describe("The org.scalatest.diagrams.Diagrams.assume(boolean) method") {
      it("should do nothing when is used to check a == 3") {
        org.scalatest.diagrams.Diagrams.assume(a == 3)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 5") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 5)
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 == b") {
        org.scalatest.diagrams.Diagrams.assume(5 == b)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 3 == b") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(3 == b)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(3 == b)
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a != 5") {
        org.scalatest.diagrams.Diagrams.assume(a != 5)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a != 3") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a != 3)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a != 3)
              |                                       | |  |
              |                                       3 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 != b") {
        org.scalatest.diagrams.Diagrams.assume(3 != b)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 5 != b") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(5 != b)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(5 != b)
              |                                       | |  |
              |                                       5 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 == 3") {
        org.scalatest.diagrams.Diagrams.assume(3 == 3)
      }

      // SKIP-DOTTY-START
      // const-folding will eliminate the expr
      it("should throw TestCanceledException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
        // This is because the compiler simply pass the false boolean literal
        // to the macro, can't find a way to get the 3 == 5 literal.
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(3 == 5)
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(3 == 5)
              |                                         |
              |                                         false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }
      // SKIP-DOTTY-END

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == b") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == b)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == b)
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      // SKIP-DOTTY-START
      it("should throw TestCanceledException with correct message and stack depth when is used to check a == null") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == null)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == null)
              |                                       | |  |
              |                                       3 |  null
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check null == a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(null == a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(null == a)
              |                                       |    |  |
              |                                       null |  3
              |                                            false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }
      // SKIP-DOTTY-END

      it("should throw TestCanceledException with correct message and stack depth when is used to check 3 != a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(3 != a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(3 != a)
              |                                       | |  |
              |                                       3 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 != a") {
        org.scalatest.diagrams.Diagrams.assume(5 != a)
      }

      it("should do nothing when is used to check a > 2") {
        org.scalatest.diagrams.Diagrams.assume(a > 2)
      }

      it("should do nothing when is used to check 5 > a") {
        org.scalatest.diagrams.Diagrams.assume(5 > a)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a > 3") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a > 3)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a > 3)
              |                                       | | |
              |                                       3 | 3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 3 > a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(3 > a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(3 > a)
              |                                       | | |
              |                                       3 | 3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a >= 3") {
        org.scalatest.diagrams.Diagrams.assume(a >= 3)
      }

      it("should do nothing when is used to check 3 >= a") {
        org.scalatest.diagrams.Diagrams.assume(3 >= a)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a >= 4") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a >= 4)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a >= 4)
              |                                       | |  |
              |                                       3 |  4
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 2 >= a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(2 >= a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(2 >= a)
              |                                       | |  |
              |                                       2 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check b < 6") {
        org.scalatest.diagrams.Diagrams.assume(b < 6)
      }

      it("should do nothing when is used to check 3 < b") {
        org.scalatest.diagrams.Diagrams.assume(3 < b)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check b < 5") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(b < 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(b < 5)
              |                                       | | |
              |                                       5 | 5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 5 < b") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(5 < b)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(5 < b)
              |                                       | | |
              |                                       5 | 5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check b <= 5") {
        org.scalatest.diagrams.Diagrams.assume(b <= 5)
      }

      it("should do nothing when is used to check 5 <= b") {
        org.scalatest.diagrams.Diagrams.assume(5 <= b)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check b <= 4") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(b <= 4)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(b <= 4)
              |                                       | |  |
              |                                       5 |  4
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 6 <= b") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(6 <= b)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(6 <= b)
              |                                       | |  |
              |                                       6 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check bob == \"bob\"") {
        org.scalatest.diagrams.Diagrams.assume(bob == "bob")
      }

      it("should do nothing when is used to check bob != \"alice\"") {
        org.scalatest.diagrams.Diagrams.assume(bob != "alice")
      }

      it("should do nothing when is used to check alice == \"alice\"") {
        org.scalatest.diagrams.Diagrams.assume(alice == "alice")
      }

      it("should do nothing when is used to check alice != \"bob\"") {
        org.scalatest.diagrams.Diagrams.assume(alice != "bob")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check bob == \"alice\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(bob == "alice")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(bob == "alice")
              |                                       |   |  |
              |                                       |   |  "alice"
              |                                       |   false
              |                                       "bob"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check bob != \"bob\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(bob != "bob")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(bob != "bob")
              |                                       |   |  |
              |                                       |   |  "bob"
              |                                       |   false
              |                                       "bob"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check alice == \"bob\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(alice == "bob")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(alice == "bob")
              |                                       |     |  |
              |                                       |     |  "bob"
              |                                       |     false
              |                                       "alice"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check alice != \"alice\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(alice != "alice")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(alice != "alice")
              |                                       |     |  |
              |                                       |     |  "alice"
              |                                       |     false
              |                                       "alice"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check a === 3") {
        org.scalatest.diagrams.Diagrams.assume(a === 3)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a === 5 ") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a === 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a === 5)
              |                                       | |   |
              |                                       3 |   5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 === a") {
        org.scalatest.diagrams.Diagrams.assume(3 === a)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 5 === a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(5 === a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(5 === a)
              |                                       | |   |
              |                                       5 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a !== 5") {
        org.scalatest.diagrams.Diagrams.assume(a !== 5)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a !== 3") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a !== 3)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a !== 3)
              |                                       | |   |
              |                                       3 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 !== a") {
        org.scalatest.diagrams.Diagrams.assume(5 !== a)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 3 !== a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(3 !== a)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(3 !== a)
              |                                       | |   |
              |                                       3 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 && b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 && b == 5)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 3 && b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 3 && b == 6)
              |                                       | |  | |  | |  |
              |                                       3 |  3 |  5 |  6
              |                                         true |    false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 5") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 && b == 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 && b == 5)
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 && b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 && b == 6)
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 & b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 & b == 5)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 & b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 3 & b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 3 & b == 6)
              |                                       | |  | | | |  |
              |                                       3 |  3 | 5 |  6
              |                                         true |   false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 5") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 & b == 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 & b == 5)
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 & b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 & b == 6)
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 || b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 || b == 5)
      }

      it("should do nothing when is used to check a == 3 || b == 6") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 || b == 6)
      }

      it("should do nothing when is used to check a == 2 || b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 2 || b == 5)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 || b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 || b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 || b == 6)
              |                                       | |  | |  | |  |
              |                                       3 |  2 |  5 |  6
              |                                         |    |    false
              |                                         |    false
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check a == 3 | b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 | b == 5)
      }

      it("should do nothing when is used to check a == 3 | b == 6") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 | b == 6)
      }

      it("should do nothing when is used to check a == 2 | b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 2 | b == 5)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 | b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 | b == 6)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 | b == 6)
              |                                       | |  | | | |  |
              |                                       3 |  2 | 5 |  6
              |                                         |    |   false
              |                                         |    false
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 && (b == 5 && b > 3))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 3 && (b == 5 && b > 5))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 3 && (b == 5 && b > 5))
              |                                       | |  | |   | |  | |  | | |
              |                                       3 |  3 |   5 |  5 |  5 | 5
              |                                         true false true |    false
              |                                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(a == 5)") {
        org.scalatest.diagrams.Diagrams.assume(!(a == 5))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(a == 3)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(a == 3))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!(a == 3))
              |                                       | | |  |
              |                                       | 3 |  3
              |                                       |   true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 3 && !(b == 5))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 3 && !(b == 5))
              |                                       | |  | |  | | |  |
              |                                       3 |  3 |  | 5 |  5
              |                                         true |  |   true
              |                                              |  false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check (a == 3) == (b == 5)") {
        org.scalatest.diagrams.Diagrams.assume((a == 3) == (b == 5))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume((a == 3) == (b != 5))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume((a == 3) == (b != 5))
              |                                        | |  |  |   | |  |
              |                                        3 |  3  |   5 |  5
              |                                          true  false false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should short-circuit && when first condition was false") {
        val s = new Stateful
        intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 5 && s.changeState)
        }
        s.state should be (false)
      }

      it("should short-circuit & when first condition was false") {
        val s = new Stateful
        intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 5 & s.changeState)
        }
        s.state should be (false)
      }

      it("should short-circuit || when first condition was true") {
        val s = new Stateful
        org.scalatest.diagrams.Diagrams.assume(a == 3 || s.changeState)
        s.state should be (false)
      }

      it("should short-circuit | when first condition was true") {
        val s = new Stateful
        org.scalatest.diagrams.Diagrams.assume(a == 3 | s.changeState)
        s.state should be (false)
      }

      it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5} ") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 && { println("hi"); b == 5})
      }

      it("should throw TestCanceledException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 3 && { println("hi"); b == 3})
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 3 && { println("hi"); b == 3})
              |                                       | |  | |                   | |  |
              |                                       3 |  3 false               5 |  3
              |                                         true                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
        org.scalatest.diagrams.Diagrams.assume({ println("hi"); b == 5} && a == 3)
      }

      it("should throw TestCanceledException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume({ println("hi"); b == 5} && a == 5)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume({ println("hi"); b == 5} && a == 5)
              |                                                        | |  |  |  | |  |
              |                                                        5 |  5  |  3 |  5
              |                                                          true  |    false
              |                                                                false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should preserve side effects when Apply with single argument is passed in") {
        org.scalatest.diagrams.Diagrams.assume(neverRuns1(sys.error("Sad times 1")))
      }

      it("should preserve side effects when Apply with 2 argument list is passed in") {
        org.scalatest.diagrams.Diagrams.assume(neverRuns2(sys.error("Sad times 2"))(0))
      }

      it("should preserve side effects when typed Apply with 2 argument list is passed in") {
        org.scalatest.diagrams.Diagrams.assume(neverRuns3(sys.error("Sad times 3"))(0))
      }

      it("should do nothing when is used to check s1 startsWith \"hi\"") {
        org.scalatest.diagrams.Diagrams.assume(s1 startsWith "hi")
        org.scalatest.diagrams.Diagrams.assume(s1.startsWith("hi"))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s2 startsWith "hi")
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(s2 startsWith "hi")
              |                                       |  |          |
              |                                       |  false      "hi"
              |                                       "ScalaTest hi"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s2.startsWith("hi"))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(s2.startsWith("hi"))
              |                                       |  |          |
              |                                       |  false      "hi"
              |                                       "ScalaTest hi"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci1 startsWith 1") {
        org.scalatest.diagrams.Diagrams.assume(ci1 startsWith 1)
        org.scalatest.diagrams.Diagrams.assume(ci1.startsWith(1))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ci2 startsWith 1") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci2 startsWith 1)
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(ci2 startsWith 1)
              |                                       |   |          |
              |                                       321 false      1
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci2.startsWith(1))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(ci2.startsWith(1))
              |                                       |   |          |
              |                                       321 false      1
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
        org.scalatest.diagrams.Diagrams.assume(!s2.startsWith("hi"))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!s1.startsWith("hi"))
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!s1.startsWith("hi"))
              |                                       ||  |          |
              |                                       ||  true       "hi"
              |                                       |"hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s2 endsWith \"hi\"") {
        org.scalatest.diagrams.Diagrams.assume(s2 endsWith "hi")
        org.scalatest.diagrams.Diagrams.assume(s2.endsWith("hi"))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s1 endsWith "hi")
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(s1 endsWith "hi")
              |                                       |  |        |
              |                                       |  false    "hi"
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s1.endsWith("hi"))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(s1.endsWith("hi"))
              |                                       |  |        |
              |                                       |  false    "hi"
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci2 endsWith 1") {
        org.scalatest.diagrams.Diagrams.assume(ci2 endsWith 1)
        org.scalatest.diagrams.Diagrams.assume(ci2.endsWith(1))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 endsWith 1") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1 endsWith 1)
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(ci1 endsWith 1)
              |                                       |   |        |
              |                                       123 false    1
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1.endsWith(1))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(ci1.endsWith(1))
              |                                       |   |        |
              |                                       123 false    1
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
        org.scalatest.diagrams.Diagrams.assume(!s1.endsWith("hi"))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!s2.endsWith("hi"))
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!s2.endsWith("hi"))
              |                                       ||  |        |
              |                                       ||  true     "hi"
              |                                       |"ScalaTest hi"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s3 contains \"hi\"") {
        org.scalatest.diagrams.Diagrams.assume(s3 contains "hi")
        org.scalatest.diagrams.Diagrams.assume(s3.contains("hi"))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s3 contains \"hello\"") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s3 contains "hello")
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(s3 contains "hello")
              |                                       |  |        |
              |                                       |  false    "hello"
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s3.contains("hello"))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(s3.contains("hello"))
              |                                       |  |        |
              |                                       |  false    "hello"
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci2 contains 2") {
        org.scalatest.diagrams.Diagrams.assume(ci2 contains 2)
        org.scalatest.diagrams.Diagrams.assume(ci2.contains(2))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 contains 5") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1 contains 5)
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(ci1 contains 5)
              |                                       |   |        |
              |                                       123 false    5
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1.contains(5))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(ci1.contains(5))
              |                                       |   |        |
              |                                       123 false    5
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s1.contains(\"hello\")") {
        org.scalatest.diagrams.Diagrams.assume(!s3.contains("hello"))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!s3.contains("hi"))
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!s3.contains("hi"))
              |                                       ||  |        |
              |                                       ||  true     "hi"
              |                                       |"Say hi to ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1 contains 2") {
        org.scalatest.diagrams.Diagrams.assume(l1 contains 2)
        org.scalatest.diagrams.Diagrams.assume(l1.contains(2))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1 contains 5") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1 contains 5)
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(l1 contains 5)
              |                                       |  |        |
              |                                       |  false    5
              |                                       List(1, 2, 3)
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.contains(5))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(l1.contains(5))
              |                                       |  |        |
              |                                       |  false    5
              |                                       List(1, 2, 3)
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !(l1 contains 5)") {
        org.scalatest.diagrams.Diagrams.assume(!(l1 contains 5))
        org.scalatest.diagrams.Diagrams.assume(!l1.contains(5))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1 contains 2)") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(l1 contains 2))
        }
        e1.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!(l1 contains 2))
              |                                       | |  |        |
              |                                       | |  true     2
              |                                       | List(1, 2, 3)
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!l1.contains(2))
        }
        e2.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!l1.contains(2))
              |                                       ||  |        |
              |                                       ||  true     2
              |                                       |List(1, 2, 3)
              |                                       false
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check m1 contains 2") {
        org.scalatest.diagrams.Diagrams.assume(m1 contains 2)
        org.scalatest.diagrams.Diagrams.assume(m1.contains(2))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check m1 contains 5") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(m1 contains 5)
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(m1 contains 5)
            |                                       |  |        |
            |                                       |  false    5
            |                                       $m1Str
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(m1.contains(5))
        }
        e2.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(m1.contains(5))
            |                                       |  |        |
            |                                       |  false    5
            |                                       $m1Str
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !(m1 contains 5)") {
        org.scalatest.diagrams.Diagrams.assume(!(m1 contains 5))
        org.scalatest.diagrams.Diagrams.assume(!m1.contains(5))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(m1 contains 2)") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(m1 contains 2))
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(!(m1 contains 2))
            |                                       | |  |        |
            |                                       | |  true     2
            |                                       | $m1Str
            |                                       false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!m1.contains(2))
        }
        e2.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(!m1.contains(2))
            |                                       ||  |        |
            |                                       ||  true     2
            |                                       |$m1Str
            |                                       false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ct1 contains 8") {
        org.scalatest.diagrams.Diagrams.assume(ct1 contains 8)
        org.scalatest.diagrams.Diagrams.assume(ct1.contains(8))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ct1 contains 5") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ct1 contains 5)
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(ct1 contains 5)
            |                                       |   |        |
            |                                       |   false    5
            |                                       $ct1Str
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ct1.contains(5))
        }
        e2.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(ct1.contains(5))
            |                                       |   |        |
            |                                       |   false    5
            |                                       $ct1Str
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ct1.contains(5)") {
        org.scalatest.diagrams.Diagrams.assume(!ct1.contains(5))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !ct1.contains(8)") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!ct1.contains(8))
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(!ct1.contains(8))
            |                                       ||   |        |
            |                                       ||   true     8
            |                                       |$ct1Str
            |                                       false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ci1 eq ci3") {
        org.scalatest.diagrams.Diagrams.assume(ci1 eq ci3)
        org.scalatest.diagrams.Diagrams.assume(ci1.eq(ci3))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 eq ci2") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1 eq ci2)
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(ci1 eq ci2)
            |                                       |   |  |
            |                                       $ci1Str |  $ci2Str
            |                                           false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1.eq(ci2))
        }
        e2.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(ci1.eq(ci2))
            |                                       |   |  |
            |                                       $ci1Str |  $ci2Str
            |                                           false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ci1.eq(ci2)") {
        org.scalatest.diagrams.Diagrams.assume(!ci1.eq(ci2))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!ci1.eq(ci3))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(!ci1.eq(ci3))
            |                                       ||   |  |
            |                                       |$ci1Str |  $ci3Str
            |                                       |    true
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ci1 ne ci2") {
        org.scalatest.diagrams.Diagrams.assume(ci1 ne ci2)
        org.scalatest.diagrams.Diagrams.assume(ci1.ne(ci2))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 ne ci3") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1 ne ci3)
        }
        e1.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(ci1 ne ci3)
            |                                       |   |  |
            |                                       $ci1Str |  $ci3Str
            |                                           false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1.ne(ci3))
        }
        e2.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(ci1.ne(ci3))
            |                                       |   |  |
            |                                       $ci1Str |  $ci3Str
            |                                           false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ci1.ne(ci3)") {
        org.scalatest.diagrams.Diagrams.assume(!ci1.ne(ci3))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!ci1.ne(ci2))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!ci1.ne(ci2))
              |                                       ||   |  |
              |                                       |123 |  321
              |                                       |    true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s4.isEmpty") {
        org.scalatest.diagrams.Diagrams.assume(s4.isEmpty)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s3.isEmpty") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s3.isEmpty)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(s3.isEmpty)
              |                                       |  |
              |                                       |  false
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !s3.isEmpty") {
        org.scalatest.diagrams.Diagrams.assume(!s3.isEmpty)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !s4.isEmpty") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!s4.isEmpty)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!s4.isEmpty)
              |                                       ||  |
              |                                       |"" true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check l2.isEmpty") {
        org.scalatest.diagrams.Diagrams.assume(l2.isEmpty)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isEmpty") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.isEmpty)
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.isEmpty)
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.isEmpty") {
        org.scalatest.diagrams.Diagrams.assume(!l1.isEmpty)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !l2.isEmpty") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!l2.isEmpty)
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(!l2.isEmpty)
            |                                       ||  |
            |                                       ||  true
            |                                       |$l2
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s1.isInstanceOf[String]") {
        org.scalatest.diagrams.Diagrams.assume(s1.isInstanceOf[String])
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.isInstanceOf[String])
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.isInstanceOf[String])
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
        org.scalatest.diagrams.Diagrams.assume(l1.isInstanceOf[List[Int]])
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s1.isInstanceOf[List[Int]])
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(s1.isInstanceOf[List[Int]])
              |                                       |  |
              |                                       |  false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check date.isInstanceOf[Date]") {
        org.scalatest.diagrams.Diagrams.assume(date.isInstanceOf[Date])
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.isInstanceOf[Date])
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.isInstanceOf[Date])
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.isInstanceOf[String]") {
        org.scalatest.diagrams.Diagrams.assume(!l1.isInstanceOf[String])
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!s1.isInstanceOf[String])
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!s1.isInstanceOf[String])
              |                                       ||  |
              |                                       ||  true
              |                                       |"hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
        org.scalatest.diagrams.Diagrams.assume(!s1.isInstanceOf[List[Int]])
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!l1.isInstanceOf[List[Int]])
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(!l1.isInstanceOf[List[Int]])
            |                                       ||  |
            |                                       ||  true
            |                                       |$l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
        org.scalatest.diagrams.Diagrams.assume(!l1.isInstanceOf[Date])
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!date.isInstanceOf[Date])
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(!date.isInstanceOf[Date])
            |                                       ||    |
            |                                       ||    true
            |                                       |$date
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s1.length == 9") {
        org.scalatest.diagrams.Diagrams.assume(s1.length == 12)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s1.length == 10") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s1.length == 10)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(s1.length == 10)
              |                                       |  |      |  |
              |                                       |  12     |  10
              |                                       |         false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1.length == 3") {
        org.scalatest.diagrams.Diagrams.assume(l1.length == 3)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.length == 10") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.length == 10)
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.length == 10)
            |                                       |  |      |  |
            |                                       |  3      |  10
            |                                       |         false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(s1.length == 10)") {
        org.scalatest.diagrams.Diagrams.assume(!(s1.length == 10))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.length == 9)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(s1.length == 12))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!(s1.length == 12))
              |                                       | |  |      |  |
              |                                       | |  12     |  12
              |                                       | |         true
              |                                       | "hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check !(l1.length == 2)") {
        org.scalatest.diagrams.Diagrams.assume(!(l1.length == 2))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.length == 9)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(l1.length == 3))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(!(l1.length == 3))
            |                                       | |  |      |  |
            |                                       | |  3      |  3
            |                                       | |         true
            |                                       | $l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check s1.size == 9") {
        org.scalatest.diagrams.Diagrams.assume(s1.size == 12)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s1.size == 10") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s1.size == 10)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(s1.size == 10)
              |                                       |  |    |  |
              |                                       |  12   |  10
              |                                       |       false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1.size == 3") {
        org.scalatest.diagrams.Diagrams.assume(l1.size == 3)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.size == 10") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.size == 10)
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.size == 10)
            |                                       |  |    |  |
            |                                       |  3    |  10
            |                                       |       false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(s1.size == 10)") {
        org.scalatest.diagrams.Diagrams.assume(!(s1.size == 10))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.size == 9)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(s1.size == 12))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(!(s1.size == 12))
              |                                       | |  |    |  |
              |                                       | |  12   |  12
              |                                       | |       true
              |                                       | "hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check !(l1.size == 2)") {
        org.scalatest.diagrams.Diagrams.assume(!(l1.size == 2))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.size == 9) ") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(l1.size == 3))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(!(l1.size == 3))
            |                                       | |  |    |  |
            |                                       | |  3    |  3
            |                                       | |       true
            |                                       | $l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check l1.exists(_ == 3)") {
        org.scalatest.diagrams.Diagrams.assume(l1.exists(_ == 3))
      }

      it("should do nothing when is used to check l1.exists(3 == _)") {
        org.scalatest.diagrams.Diagrams.assume(l1.exists(3 == _))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ == 5) ") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.exists(_ == 5))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.exists(_ == 5))
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(5 == _) ") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.exists(5 == _))
        }
        e.message should be (
          Some(
            s"""
               |
               |org.scalatest.diagrams.Diagrams.assume(l1.exists(5 == _))
               |                                       |  |
               |                                       |  false
               |                                       $l1
               |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.exists(_ == 5)") {
        org.scalatest.diagrams.Diagrams.assume(!l1.exists(_ == 5))
      }

      it("should do nothing when is used to check !l1.exists(5 == _)") {
        org.scalatest.diagrams.Diagrams.assume(!l1.exists(5 == _))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!l1.exists(_ == 3))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(!l1.exists(_ == 3))
            |                                       ||  |
            |                                       ||  true
            |                                       |$l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!l1.exists(3 == _))
        }
        e.message should be (
          Some(
            s"""
               |
               |org.scalatest.diagrams.Diagrams.assume(!l1.exists(3 == _))
               |                                       ||  |
               |                                       ||  true
               |                                       |$l1
               |                                       false
               |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.exists(_ > 3))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.exists(_ > 3))
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(3 < _)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.exists(3 < _))
        }
        e.message should be (
          Some(
            s"""
               |
               |org.scalatest.diagrams.Diagrams.assume(l1.exists(3 < _))
               |                                       |  |
               |                                       |  false
               |                                       $l1
               |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l3.exists(_.isEmpty))
        }
        e.message should be (
          Some(
            s"""
            |
            |org.scalatest.diagrams.Diagrams.assume(l3.exists(_.isEmpty))
            |                                       |  |
            |                                       |  false
            |                                       $l3Str
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(false)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1.exists(321))
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(ci1.exists(321))
              |                                       |   |      |
              |                                       123 false  321
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
        org.scalatest.diagrams.Diagrams.assume(woof { meow(y = 5) } == "woof")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(woof { meow(y = 5) } == "meow")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(woof { meow(y = 5) } == "meow")
              |                                       |                    |  |
              |                                       "woof"               |  "meow"
              |                                                            false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when used to check multiline org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 2 <= a)) ") {
        org.scalatest.diagrams.Diagrams.assume((b == a + 2) && (b - 2 <=
          a))
      }

      // SKIP-DOTTY-START
      // problem with quotes
      it("should throw TestCanceledException with friend message when used to check multiline org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 1 <= a))") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume((b == a + 2) && (b - 1 <=
            a))
        }
        e.message shouldBe Some("5 equaled 5, but 4 was not less than or equal to 3")
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
      }
      // SKIP-DOTTY-END

      it("should do nothing when a block of code that evaluates to true is passed in") {
        assume {
          val a = 1
          val b = 2
          val c = a + b
          a < b || c == a + b
        }
      }

      it("should throw TestCanceledException with correct message and stack depth when a block of code that evaluates to false is passed") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume { val a = 1; val b = 2; val c = a + b; a > b || c == b * b }
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume { val a = 1; val b = 2; val c = a + b; a > b || c == b * b }
              |                                                                              | | | |  | |  | | |
              |                                                                              1 | 2 |  3 |  2 4 2
              |                                                                                |   |    false
              |                                                                                |   false
              |                                                                                false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      // SKIP-DOTTY-START
      // different code show in Dotty
      it("should fallback to BooleanMacro when a block of code > 1 line is passed in ") {
        val e = intercept[TestCanceledException] {
          assume {
            val a = 1
            val b = 2
            val c = a + b
            a > b || c == b * b }
        }
        e.message should be (
          Some(
            """{
              |  val a: Int = 1;
              |  val b: Int = 2;
              |  val c: Int = a.+(b);
              |  a.>(b).||(c.==(b.*(b)))
              |} was false""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 17))
      }
      // SKIP-DOTTY-END

      // SKIP-SCALATESTJS,NATIVE-START
      it("should do nothing when used to check <person>Dude</person> == <person>Dude</person>") {
        org.scalatest.diagrams.Diagrams.assume(<person>Dude</person> == <person>Dude</person>)
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check <person>Dude</person> == <person>Mary</person>") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(<person>Dude</person> == <person>Mary</person>)
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(<person>Dude</person> == <person>Mary</person>)
              |                                       |                     |  |
              |                                       <person>Dude</person> |  <person>Mary</person>
              |                                                             false
              |""".stripMargin
          )
        )
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should compile when used with org == xxx that shadow org.scalactic ") {
        assertCompiles(
          """
            |val org = "test"
            |_root_.org.scalatest.diagrams.Diagrams.assume(org == "test")
          """.stripMargin)
      }

      it("should compile when used with org === xxx that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = "test"
            |_root_.org.scalatest.diagrams.Diagrams.assume(org === "test")
          """.stripMargin)
      }

      it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
        assertCompiles(
          """
            |class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
            |  it("testing here") {
            |    val org = "test"
            |    _root_.org.scalatest.diagrams.Diagrams.assume(org === "test")
            |  }
            |}
          """.stripMargin)
      }

      it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
        assertCompiles(
          """
            |class Test {
            |  def aCustomMethod: Boolean = true
            |}
            |val org = new Test
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.aCustomMethod)
          """.stripMargin)
      }

      it("should compile when used with !org that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = false
            |_root_.org.scalatest.diagrams.Diagrams.assume(!org)
          """.stripMargin)
      }

      it("should compile when used with org.isEmpty that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.isEmpty)
          """.stripMargin)
      }

      it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.isInstanceOf[String])
          """.stripMargin)
      }

      it("should compile when used with org.size == 0 that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = Array.empty[String]
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.size == 0)
          """.stripMargin)
      }

      it("should compile when used with org.length == 0 that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.length == 0)
          """.stripMargin)
      }

      it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
        assertCompiles(
          """
            |val org = "abc"
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.exists(_ == 'b'))
          """.stripMargin)
      }

      it("should do nothing when is used to check new String(\"test\") != \"test\"") {
        org.scalatest.diagrams.Diagrams.assume(new String("test") == "test")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check new String(\"test\") != \"testing\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(new String("test") == "testing")
        }
        e.message should be (
          Some(
            """
              |
              |org.scalatest.diagrams.Diagrams.assume(new String("test") == "testing")
              |                                       |                  |  |
              |                                       "test"             |  "testing"
              |                                                          false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should compile when used with Java static method") {
        assertCompiles(
          """
            |org.scalatest.diagrams.Diagrams.assume(System.currentTimeMillis() > 0)
          """.stripMargin)
        assertCompiles(
          """
            |org.scalatest.diagrams.Diagrams.assume(java.math.BigDecimal.ZERO == java.math.BigDecimal.ZERO)
          """.stripMargin)
      }
    }

    describe("The org.scalatest.diagrams.Diagrams.assume(boolean, clue) method") {
      it("should do nothing when is used to check a == 3") {
        org.scalatest.diagrams.Diagrams.assume(a == 3, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 5") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 5, "this is a clue")
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 == b") {
        org.scalatest.diagrams.Diagrams.assume(5 == b, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 3 == b") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(3 == b, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(3 == b, "this is a clue")
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a != 5") {
        org.scalatest.diagrams.Diagrams.assume(a != 5, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a != 3") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a != 3, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a != 3, "this is a clue")
              |                                       | |  |
              |                                       3 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 != b") {
        org.scalatest.diagrams.Diagrams.assume(3 != b, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 5 != b") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(5 != b, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(5 != b, "this is a clue")
              |                                       | |  |
              |                                       5 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 == 3") {
        org.scalatest.diagrams.Diagrams.assume(3 == 3, "this is a clue")
      }

      // SKIP-DOTTY-START
      // const-folding will eliminate the expr
      it("should throw TestCanceledException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
        // This is because the compiler simply pass the false boolean literal
        // to the macro, can't find a way to get the 3 == 5 literal.
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(3 == 5, "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(3 == 5, "this is a clue")
              |                                         |
              |                                         false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }
      // SKIP-DOTTY-END

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == b") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == b, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == b, "this is a clue")
              |                                       | |  |
              |                                       3 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      // SKIP-DOTTY-START
      it("should throw TestCanceledException with correct message and stack depth when is used to check a == null") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == null, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == null, "this is a clue")
              |                                       | |  |
              |                                       3 |  null
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check null == a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(null == a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(null == a, "this is a clue")
              |                                       |    |  |
              |                                       null |  3
              |                                            false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }
      // SKIP-DOTTY-END

      it("should throw TestCanceledException with correct message and stack depth when is used to check 3 != a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(3 != a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(3 != a, "this is a clue")
              |                                       | |  |
              |                                       3 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 != a") {
        org.scalatest.diagrams.Diagrams.assume(5 != a, "this is a clue")
      }

      it("should do nothing when is used to check a > 2") {
        org.scalatest.diagrams.Diagrams.assume(a > 2, "this is a clue")
      }

      it("should do nothing when is used to check 5 > a") {
        org.scalatest.diagrams.Diagrams.assume(5 > a, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a > 3") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a > 3, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a > 3, "this is a clue")
              |                                       | | |
              |                                       3 | 3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 3 > a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(3 > a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(3 > a, "this is a clue")
              |                                       | | |
              |                                       3 | 3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a >= 3") {
        org.scalatest.diagrams.Diagrams.assume(a >= 3, "this is a clue")
      }

      it("should do nothing when is used to check 3 >= a") {
        org.scalatest.diagrams.Diagrams.assume(3 >= a, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a >= 4") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a >= 4, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a >= 4, "this is a clue")
              |                                       | |  |
              |                                       3 |  4
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 2 >= a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(2 >= a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(2 >= a, "this is a clue")
              |                                       | |  |
              |                                       2 |  3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check b < 6") {
        org.scalatest.diagrams.Diagrams.assume(b < 6, "this is a clue")
      }

      it("should do nothing when is used to check 3 < b") {
        org.scalatest.diagrams.Diagrams.assume(3 < b, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check b < 5") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(b < 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(b < 5, "this is a clue")
              |                                       | | |
              |                                       5 | 5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 5 < b") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(5 < b, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(5 < b, "this is a clue")
              |                                       | | |
              |                                       5 | 5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check b <= 5") {
        org.scalatest.diagrams.Diagrams.assume(b <= 5, "this is a clue")
      }

      it("should do nothing when is used to check 5 <= b") {
        org.scalatest.diagrams.Diagrams.assume(5 <= b, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check b <= 4") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(b <= 4, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(b <= 4, "this is a clue")
              |                                       | |  |
              |                                       5 |  4
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 6 <= b") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(6 <= b, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(6 <= b, "this is a clue")
              |                                       | |  |
              |                                       6 |  5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check bob == \"bob\"") {
        org.scalatest.diagrams.Diagrams.assume(bob == "bob", "this is a clue")
      }

      it("should do nothing when is used to check bob != \"alice\"") {
        org.scalatest.diagrams.Diagrams.assume(bob != "alice", "this is a clue")
      }

      it("should do nothing when is used to check alice == \"alice\"") {
        org.scalatest.diagrams.Diagrams.assume(alice == "alice", "this is a clue")
      }

      it("should do nothing when is used to check alice != \"bob\"") {
        org.scalatest.diagrams.Diagrams.assume(alice != "bob", "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check bob == \"alice\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(bob == "alice", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(bob == "alice", "this is a clue")
              |                                       |   |  |
              |                                       |   |  "alice"
              |                                       |   false
              |                                       "bob"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check bob != \"bob\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(bob != "bob", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(bob != "bob", "this is a clue")
              |                                       |   |  |
              |                                       |   |  "bob"
              |                                       |   false
              |                                       "bob"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check alice == \"bob\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(alice == "bob", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(alice == "bob", "this is a clue")
              |                                       |     |  |
              |                                       |     |  "bob"
              |                                       |     false
              |                                       "alice"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check alice != \"alice\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(alice != "alice", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(alice != "alice", "this is a clue")
              |                                       |     |  |
              |                                       |     |  "alice"
              |                                       |     false
              |                                       "alice"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check a === 3") {
        org.scalatest.diagrams.Diagrams.assume(a === 3, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a === 5 ") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a === 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a === 5, "this is a clue")
              |                                       | |   |
              |                                       3 |   5
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 3 === a") {
        org.scalatest.diagrams.Diagrams.assume(3 === a, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 5 === a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(5 === a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(5 === a, "this is a clue")
              |                                       | |   |
              |                                       5 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a !== 5") {
        org.scalatest.diagrams.Diagrams.assume(a !== 5, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a !== 3") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a !== 3, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a !== 3, "this is a clue")
              |                                       | |   |
              |                                       3 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check 5 !== a") {
        org.scalatest.diagrams.Diagrams.assume(5 !== a, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check 3 !== a") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(3 !== a, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(3 !== a, "this is a clue")
              |                                       | |   |
              |                                       3 |   3
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 && b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 && b == 5, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 3 && b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 3 && b == 6, "this is a clue")
              |                                       | |  | |  | |  |
              |                                       3 |  3 |  5 |  6
              |                                         true |    false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 5") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 && b == 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 && b == 5, "this is a clue")
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 && b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 && b == 6, "this is a clue")
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 & b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 & b == 5, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 & b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 3 & b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 3 & b == 6, "this is a clue")
              |                                       | |  | | | |  |
              |                                       3 |  3 | 5 |  6
              |                                         true |   false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 5") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 & b == 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 & b == 5, "this is a clue")
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 & b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 & b == 6, "this is a clue")
              |                                       | |  |
              |                                       3 |  2
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check a == 3 || b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 || b == 5, "this is a clue")
      }

      it("should do nothing when is used to check a == 3 || b == 6") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 || b == 6, "this is a clue")
      }

      it("should do nothing when is used to check a == 2 || b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 2 || b == 5, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 || b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 || b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 || b == 6, "this is a clue")
              |                                       | |  | |  | |  |
              |                                       3 |  2 |  5 |  6
              |                                         |    |    false
              |                                         |    false
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check a == 3 | b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 | b == 5, "this is a clue")
      }

      it("should do nothing when is used to check a == 3 | b == 6") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 | b == 6, "this is a clue")
      }

      it("should do nothing when is used to check a == 2 | b == 5") {
        org.scalatest.diagrams.Diagrams.assume(a == 2 | b == 5, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 | b == 6") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 2 | b == 6, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 2 | b == 6, "this is a clue")
              |                                       | |  | | | |  |
              |                                       3 |  2 | 5 |  6
              |                                         |    |   false
              |                                         |    false
              |                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 && (b == 5 && b > 3), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 3 && (b == 5 && b > 5), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 3 && (b == 5 && b > 5), "this is a clue")
              |                                       | |  | |   | |  | |  | | |
              |                                       3 |  3 |   5 |  5 |  5 | 5
              |                                         true false true |    false
              |                                                         false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(a == 5)") {
        org.scalatest.diagrams.Diagrams.assume(!(a == 5), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(a == 3)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(a == 3), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!(a == 3), "this is a clue")
              |                                       | | |  |
              |                                       | 3 |  3
              |                                       |   true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 3 && !(b == 5), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 3 && !(b == 5), "this is a clue")
              |                                       | |  | |  | | |  |
              |                                       3 |  3 |  | 5 |  5
              |                                         true |  |   true
              |                                              |  false
              |                                              false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check (a == 3) == (b == 5)") {
        org.scalatest.diagrams.Diagrams.assume((a == 3) == (b == 5), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume((a == 3) == (b != 5), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume((a == 3) == (b != 5), "this is a clue")
              |                                        | |  |  |   | |  |
              |                                        3 |  3  |   5 |  5
              |                                          true  false false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should short-circuit && when first condition was false") {
        val s = new Stateful
        intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 5 && s.changeState, "this is a clue")
        }
        s.state should be (false)
      }

      it("should short-circuit & when first condition was false") {
        val s = new Stateful
        intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 5 & s.changeState, "this is a clue")
        }
        s.state should be (false)
      }

      it("should short-circuit || when first condition was true") {
        val s = new Stateful
        org.scalatest.diagrams.Diagrams.assume(a == 3 || s.changeState, "this is a clue")
        s.state should be (false)
      }

      it("should short-circuit | when first condition was true") {
        val s = new Stateful
        org.scalatest.diagrams.Diagrams.assume(a == 3 | s.changeState, "this is a clue")
        s.state should be (false)
      }

      it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5} ") {
        org.scalatest.diagrams.Diagrams.assume(a == 3 && { println("hi"); b == 5}, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(a == 3 && { println("hi"); b == 3}, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(a == 3 && { println("hi"); b == 3}, "this is a clue")
              |                                       | |  | |                   | |  |
              |                                       3 |  3 false               5 |  3
              |                                         true                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
        org.scalatest.diagrams.Diagrams.assume({ println("hi"); b == 5} && a == 3, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume({ println("hi"); b == 5} && a == 5, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume({ println("hi"); b == 5} && a == 5, "this is a clue")
              |                                                        | |  |  |  | |  |
              |                                                        5 |  5  |  3 |  5
              |                                                          true  |    false
              |                                                                false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should preserve side effects when Apply with single argument is passed in") {
        org.scalatest.diagrams.Diagrams.assume(neverRuns1(sys.error("Sad times 1")), "this is a clue")
      }

      it("should preserve side effects when Apply with 2 argument list is passed in") {
        org.scalatest.diagrams.Diagrams.assume(neverRuns2(sys.error("Sad times 2"))(0), "this is a clue")
      }

      it("should preserve side effects when typed Apply with 2 argument list is passed in") {
        org.scalatest.diagrams.Diagrams.assume(neverRuns3(sys.error("Sad times 3"))(0), "this is a clue")
      }

      it("should do nothing when is used to check s1 startsWith \"hi\"") {
        org.scalatest.diagrams.Diagrams.assume(s1 startsWith "hi", "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(s1.startsWith("hi"), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s2 startsWith "hi", "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(s2 startsWith "hi", "this is a clue")
              |                                       |  |          |
              |                                       |  false      "hi"
              |                                       "ScalaTest hi"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s2.startsWith("hi"), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(s2.startsWith("hi"), "this is a clue")
              |                                       |  |          |
              |                                       |  false      "hi"
              |                                       "ScalaTest hi"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci1 startsWith 1") {
        org.scalatest.diagrams.Diagrams.assume(ci1 startsWith 1, "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(ci1.startsWith(1), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ci2 startsWith 1") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci2 startsWith 1, "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(ci2 startsWith 1, "this is a clue")
              |                                       |   |          |
              |                                       321 false      1
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci2.startsWith(1), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(ci2.startsWith(1), "this is a clue")
              |                                       |   |          |
              |                                       321 false      1
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
        org.scalatest.diagrams.Diagrams.assume(!s2.startsWith("hi"), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!s1.startsWith("hi"), "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!s1.startsWith("hi"), "this is a clue")
              |                                       ||  |          |
              |                                       ||  true       "hi"
              |                                       |"hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s2 endsWith \"hi\"") {
        org.scalatest.diagrams.Diagrams.assume(s2 endsWith "hi", "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(s2.endsWith("hi"), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s1 endsWith "hi", "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(s1 endsWith "hi", "this is a clue")
              |                                       |  |        |
              |                                       |  false    "hi"
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s1.endsWith("hi"), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(s1.endsWith("hi"), "this is a clue")
              |                                       |  |        |
              |                                       |  false    "hi"
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci2 endsWith 1") {
        org.scalatest.diagrams.Diagrams.assume(ci2 endsWith 1, "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(ci2.endsWith(1), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 endsWith 1") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1 endsWith 1, "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(ci1 endsWith 1, "this is a clue")
              |                                       |   |        |
              |                                       123 false    1
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1.endsWith(1), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(ci1.endsWith(1), "this is a clue")
              |                                       |   |        |
              |                                       123 false    1
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
        org.scalatest.diagrams.Diagrams.assume(!s1.endsWith("hi"), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!s2.endsWith("hi"), "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!s2.endsWith("hi"), "this is a clue")
              |                                       ||  |        |
              |                                       ||  true     "hi"
              |                                       |"ScalaTest hi"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s3 contains \"hi\"") {
        org.scalatest.diagrams.Diagrams.assume(s3 contains "hi", "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(s3.contains("hi"), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s3 contains \"hello\"") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s3 contains "hello", "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(s3 contains "hello", "this is a clue")
              |                                       |  |        |
              |                                       |  false    "hello"
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s3.contains("hello"), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(s3.contains("hello"), "this is a clue")
              |                                       |  |        |
              |                                       |  false    "hello"
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check ci2 contains 2") {
        org.scalatest.diagrams.Diagrams.assume(ci2 contains 2, "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(ci2.contains(2), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 contains 5") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1 contains 5, "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(ci1 contains 5, "this is a clue")
              |                                       |   |        |
              |                                       123 false    5
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 13))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1.contains(5), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(ci1.contains(5), "this is a clue")
              |                                       |   |        |
              |                                       123 false    5
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when is used to check !s1.contains(\"hello\")") {
        org.scalatest.diagrams.Diagrams.assume(!s3.contains("hello"), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!s3.contains("hi"), "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!s3.contains("hi"), "this is a clue")
              |                                       ||  |        |
              |                                       ||  true     "hi"
              |                                       |"Say hi to ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1 contains 2") {
        org.scalatest.diagrams.Diagrams.assume(l1 contains 2, "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(l1.contains(2), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1 contains 5") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1 contains 5, "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(l1 contains 5, "this is a clue")
              |                                       |  |        |
              |                                       |  false    5
              |                                       List(1, 2, 3)
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.contains(5), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(l1.contains(5), "this is a clue")
              |                                       |  |        |
              |                                       |  false    5
              |                                       List(1, 2, 3)
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !(l1 contains 5)") {
        org.scalatest.diagrams.Diagrams.assume(!(l1 contains 5), "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(!l1.contains(5), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1 contains 2)") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(l1 contains 2), "this is a clue")
        }
        e1.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!(l1 contains 2), "this is a clue")
              |                                       | |  |        |
              |                                       | |  true     2
              |                                       | List(1, 2, 3)
              |                                       false
              |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!l1.contains(2), "this is a clue")
        }
        e2.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!l1.contains(2), "this is a clue")
              |                                       ||  |        |
              |                                       ||  true     2
              |                                       |List(1, 2, 3)
              |                                       false
              |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check m1 contains 2") {
        org.scalatest.diagrams.Diagrams.assume(m1 contains 2, "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(m1.contains(2), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check m1 contains 5") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(m1 contains 5, "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(m1 contains 5, "this is a clue")
            |                                       |  |        |
            |                                       |  false    5
            |                                       $m1Str
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(m1.contains(5), "this is a clue")
        }
        e2.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(m1.contains(5), "this is a clue")
            |                                       |  |        |
            |                                       |  false    5
            |                                       $m1Str
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !(m1 contains 5)") {
        org.scalatest.diagrams.Diagrams.assume(!(m1 contains 5), "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(!m1.contains(5), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(m1 contains 2)") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(m1 contains 2), "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(!(m1 contains 2), "this is a clue")
            |                                       | |  |        |
            |                                       | |  true     2
            |                                       | $m1Str
            |                                       false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!m1.contains(2), "this is a clue")
        }
        e2.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(!m1.contains(2), "this is a clue")
            |                                       ||  |        |
            |                                       ||  true     2
            |                                       |$m1Str
            |                                       false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ct1 contains 8") {
        org.scalatest.diagrams.Diagrams.assume(ct1 contains 8, "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(ct1.contains(8), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ct1 contains 5") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ct1 contains 5, "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(ct1 contains 5, "this is a clue")
            |                                       |   |        |
            |                                       |   false    5
            |                                       $ct1Str
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ct1.contains(5), "this is a clue")
        }
        e2.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(ct1.contains(5), "this is a clue")
            |                                       |   |        |
            |                                       |   false    5
            |                                       $ct1Str
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ct1.contains(5)") {
        org.scalatest.diagrams.Diagrams.assume(!ct1.contains(5), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !ct1.contains(8)") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!ct1.contains(8), "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(!ct1.contains(8), "this is a clue")
            |                                       ||   |        |
            |                                       ||   true     8
            |                                       |$ct1Str
            |                                       false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ci1 eq ci3") {
        org.scalatest.diagrams.Diagrams.assume(ci1 eq ci3, "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(ci1.eq(ci3), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 eq ci2") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1 eq ci2, "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(ci1 eq ci2, "this is a clue")
            |                                       |   |  |
            |                                       $ci1Str |  $ci2Str
            |                                           false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1.eq(ci2), "this is a clue")
        }
        e2.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(ci1.eq(ci2), "this is a clue")
            |                                       |   |  |
            |                                       $ci1Str |  $ci2Str
            |                                           false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ci1.eq(ci2)") {
        org.scalatest.diagrams.Diagrams.assume(!ci1.eq(ci2), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!ci1.eq(ci3), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(!ci1.eq(ci3), "this is a clue")
            |                                       ||   |  |
            |                                       |$ci1Str |  $ci3Str
            |                                       |    true
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check ci1 ne ci2") {
        org.scalatest.diagrams.Diagrams.assume(ci1 ne ci2, "this is a clue")
        org.scalatest.diagrams.Diagrams.assume(ci1.ne(ci2), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 ne ci3") {
        val e1 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1 ne ci3, "this is a clue")
        }
        e1.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(ci1 ne ci3, "this is a clue")
            |                                       |   |  |
            |                                       $ci1Str |  $ci3Str
            |                                           false
            |""".stripMargin
          )
        )
        e1.failedCodeFileName should be (Some(fileName))
        e1.failedCodeLineNumber should be (Some(thisLineNumber - 14))

        val e2 = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1.ne(ci3), "this is a clue")
        }
        e2.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(ci1.ne(ci3), "this is a clue")
            |                                       |   |  |
            |                                       $ci1Str |  $ci3Str
            |                                           false
            |""".stripMargin
          )
        )
        e2.failedCodeFileName should be (Some(fileName))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !ci1.ne(ci3)") {
        org.scalatest.diagrams.Diagrams.assume(!ci1.ne(ci3), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!ci1.ne(ci2), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!ci1.ne(ci2), "this is a clue")
              |                                       ||   |  |
              |                                       |123 |  321
              |                                       |    true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s4.isEmpty") {
        org.scalatest.diagrams.Diagrams.assume(s4.isEmpty, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s3.isEmpty") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s3.isEmpty, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(s3.isEmpty, "this is a clue")
              |                                       |  |
              |                                       |  false
              |                                       "Say hi to ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !s3.isEmpty") {
        org.scalatest.diagrams.Diagrams.assume(!s3.isEmpty, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !s4.isEmpty") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!s4.isEmpty, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!s4.isEmpty, "this is a clue")
              |                                       ||  |
              |                                       |"" true
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check l2.isEmpty") {
        org.scalatest.diagrams.Diagrams.assume(l2.isEmpty, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isEmpty") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.isEmpty, "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.isEmpty, "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.isEmpty") {
        org.scalatest.diagrams.Diagrams.assume(!l1.isEmpty, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !l2.isEmpty") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!l2.isEmpty, "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(!l2.isEmpty, "this is a clue")
            |                                       ||  |
            |                                       ||  true
            |                                       |$l2
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s1.isInstanceOf[String]") {
        org.scalatest.diagrams.Diagrams.assume(s1.isInstanceOf[String], "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.isInstanceOf[String], "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.isInstanceOf[String], "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
        org.scalatest.diagrams.Diagrams.assume(l1.isInstanceOf[List[Int]], "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s1.isInstanceOf[List[Int]], "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(s1.isInstanceOf[List[Int]], "this is a clue")
              |                                       |  |
              |                                       |  false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check date.isInstanceOf[Date]") {
        org.scalatest.diagrams.Diagrams.assume(date.isInstanceOf[Date], "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.isInstanceOf[Date], "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.isInstanceOf[Date], "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.isInstanceOf[String]") {
        org.scalatest.diagrams.Diagrams.assume(!l1.isInstanceOf[String], "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!s1.isInstanceOf[String], "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!s1.isInstanceOf[String], "this is a clue")
              |                                       ||  |
              |                                       ||  true
              |                                       |"hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
        org.scalatest.diagrams.Diagrams.assume(!s1.isInstanceOf[List[Int]], "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!l1.isInstanceOf[List[Int]], "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(!l1.isInstanceOf[List[Int]], "this is a clue")
            |                                       ||  |
            |                                       ||  true
            |                                       |$l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
        org.scalatest.diagrams.Diagrams.assume(!l1.isInstanceOf[Date], "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!date.isInstanceOf[Date], "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(!date.isInstanceOf[Date], "this is a clue")
            |                                       ||    |
            |                                       ||    true
            |                                       |$date
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check s1.length == 9") {
        org.scalatest.diagrams.Diagrams.assume(s1.length == 12, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s1.length == 10") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s1.length == 10, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(s1.length == 10, "this is a clue")
              |                                       |  |      |  |
              |                                       |  12     |  10
              |                                       |         false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1.length == 3") {
        org.scalatest.diagrams.Diagrams.assume(l1.length == 3, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.length == 10") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.length == 10, "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.length == 10, "this is a clue")
            |                                       |  |      |  |
            |                                       |  3      |  10
            |                                       |         false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(s1.length == 10)") {
        org.scalatest.diagrams.Diagrams.assume(!(s1.length == 10), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.length == 9)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(s1.length == 12), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!(s1.length == 12), "this is a clue")
              |                                       | |  |      |  |
              |                                       | |  12     |  12
              |                                       | |         true
              |                                       | "hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check !(l1.length == 2)") {
        org.scalatest.diagrams.Diagrams.assume(!(l1.length == 2), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.length == 9)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(l1.length == 3), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(!(l1.length == 3), "this is a clue")
            |                                       | |  |      |  |
            |                                       | |  3      |  3
            |                                       | |         true
            |                                       | $l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check s1.size == 9") {
        org.scalatest.diagrams.Diagrams.assume(s1.size == 12, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check s1.size == 10") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(s1.size == 10, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(s1.size == 10, "this is a clue")
              |                                       |  |    |  |
              |                                       |  12   |  10
              |                                       |       false
              |                                       "hi ScalaTest"
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check l1.size == 3") {
        org.scalatest.diagrams.Diagrams.assume(l1.size == 3, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.size == 10") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.size == 10, "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.size == 10, "this is a clue")
            |                                       |  |    |  |
            |                                       |  3    |  10
            |                                       |       false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should do nothing when is used to check !(s1.size == 10)") {
        org.scalatest.diagrams.Diagrams.assume(!(s1.size == 10), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.size == 9)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(s1.size == 12), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(!(s1.size == 12), "this is a clue")
              |                                       | |  |    |  |
              |                                       | |  12   |  12
              |                                       | |       true
              |                                       | "hi ScalaTest"
              |                                       false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check !(l1.size == 2)") {
        org.scalatest.diagrams.Diagrams.assume(!(l1.size == 2), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.size == 9) ") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!(l1.size == 3), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(!(l1.size == 3), "this is a clue")
            |                                       | |  |    |  |
            |                                       | |  3    |  3
            |                                       | |       true
            |                                       | $l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      it("should do nothing when is used to check l1.exists(_ == 3)") {
        org.scalatest.diagrams.Diagrams.assume(l1.exists(_ == 3), "this is a clue")
      }

      it("should do nothing when is used to check l1.exists(3 == _)") {
        org.scalatest.diagrams.Diagrams.assume(l1.exists(3 == _), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ == 5) ") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.exists(_ == 5), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.exists(_ == 5), "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(5 == _) ") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.exists(5 == _), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
               |
               |org.scalatest.diagrams.Diagrams.assume(l1.exists(5 == _), "this is a clue")
               |                                       |  |
               |                                       |  false
               |                                       $l1
               |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when is used to check !l1.exists(_ == 5)") {
        org.scalatest.diagrams.Diagrams.assume(!l1.exists(_ == 5), "this is a clue")
      }

      it("should do nothing when is used to check !l1.exists(5 == _)") {
        org.scalatest.diagrams.Diagrams.assume(!l1.exists(5 == _), "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!l1.exists(_ == 3), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(!l1.exists(_ == 3), "this is a clue")
            |                                       ||  |
            |                                       ||  true
            |                                       |$l1
            |                                       false
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(!l1.exists(3 == _), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
               |
               |org.scalatest.diagrams.Diagrams.assume(!l1.exists(3 == _), "this is a clue")
               |                                       ||  |
               |                                       ||  true
               |                                       |$l1
               |                                       false
               |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.exists(_ > 3), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(l1.exists(_ > 3), "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l1
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(3 < _)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l1.exists(3 < _), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
               |
               |org.scalatest.diagrams.Diagrams.assume(l1.exists(3 < _), "this is a clue")
               |                                       |  |
               |                                       |  false
               |                                       $l1
               |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(l3.exists(_.isEmpty), "this is a clue")
        }
        e.message should be (
          Some(
            s"""this is a clue
            |
            |org.scalatest.diagrams.Diagrams.assume(l3.exists(_.isEmpty), "this is a clue")
            |                                       |  |
            |                                       |  false
            |                                       $l3Str
            |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(false)") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(ci1.exists(321), "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(ci1.exists(321), "this is a clue")
              |                                       |   |      |
              |                                       123 false  321
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
      }

      it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
        org.scalatest.diagrams.Diagrams.assume(woof { meow(y = 5) } == "woof", "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(woof { meow(y = 5) } == "meow", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(woof { meow(y = 5) } == "meow", "this is a clue")
              |                                       |                    |  |
              |                                       "woof"               |  "meow"
              |                                                            false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should do nothing when used to check multiline org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 2 <= a)) ") {
        org.scalatest.diagrams.Diagrams.assume((b == a + 2) && (b - 2 <=
          a), "this is a clue")
      }

      // SKIP-DOTTY-START
      // problem with quotes
      it("should throw friend message when used to check multiline org.scalatest.diagrams.Diagrams.assert((b == a + 2) && (b - 1 <= a))") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume((b == a + 2) && (b - 1 <=
            a), "this is a clue")
        }
        e.message shouldBe Some("5 equaled 5, but 4 was not less than or equal to 3 this is a clue")
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
      }
      // SKIP-DOTTY-END

      it("should do nothing when a block of code that evaluates to true is passed in") {
        org.scalatest.diagrams.Diagrams.assume({
          val a = 1
          val b = 2
          val c = a + b
          a < b || c == a + b
        }, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when a block of code that evaluates to false is passed") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume({ val a = 1; val b = 2; val c = a + b; a > b || c == b * b }, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume({ val a = 1; val b = 2; val c = a + b; a > b || c == b * b }, "this is a clue")
              |                                                                              | | | |  | |  | | |
              |                                                                              1 | 2 |  3 |  2 4 2
              |                                                                                |   |    false
              |                                                                                |   false
              |                                                                                false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 16))
      }

      // SKIP-DOTTY-START
      // different code show in Dotty
      it("should fallback to BooleanMacro when a block of code > 1 line is passed in ") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume({
            val a = 1
            val b = 2
            val c = a + b
            a > b || c == b * b }, "this is a clue")
        }
        e.message should be (
          Some(
            """{
              |  val a: Int = 1;
              |  val b: Int = 2;
              |  val c: Int = a.+(b);
              |  a.>(b).||(c.==(b.*(b)))
              |} was false this is a clue""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 17))
      }
      // SKIP-DOTTY-END

      // SKIP-SCALATESTJS,NATIVE-START
      it("should do nothing when used to check <person>Dude</person> == <person>Dude</person>") {
        org.scalatest.diagrams.Diagrams.assume(<person>Dude</person> == <person>Dude</person>, "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check <person>Dude</person> == <person>Mary</person>") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(<person>Dude</person> == <person>Mary</person>, "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(<person>Dude</person> == <person>Mary</person>, "this is a clue")
              |                                       |                     |  |
              |                                       <person>Dude</person> |  <person>Mary</person>
              |                                                             false
              |""".stripMargin
          )
        )
      }
      // SKIP-SCALATESTJS,NATIVE-END

      it("should compile when used with org == xxx that shadow org.scalactic ") {
        assertCompiles(
          """
            |val org = "test"
            |_root_.org.scalatest.diagrams.Diagrams.assume(org == "test", "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org === xxx that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = "test"
            |_root_.org.scalatest.diagrams.Diagrams.assume(org === "test", "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
        assertCompiles(
          """
            |class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
            |  it("testing here") {
            |    val org = "test"
            |    _root_.org.scalatest.diagrams.Diagrams.assume(org === "test", "this is a clue")
            |  }
            |}
          """.stripMargin)
      }

      it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
        assertCompiles(
          """
            |class Test {
            |  def aCustomMethod: Boolean = true
            |}
            |val org = new Test
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.aCustomMethod, "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with !org that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = false
            |_root_.org.scalatest.diagrams.Diagrams.assume(!org, "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org.isEmpty that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.isEmpty, "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.isInstanceOf[String], "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org.size == 0 that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = Array.empty[String]
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.size == 0, "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org.length == 0 that shadow org.scalactic") {
        assertCompiles(
          """
            |val org = ""
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.length == 0, "this is a clue")
          """.stripMargin)
      }

      it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
        assertCompiles(
          """
            |val org = "abc"
            |_root_.org.scalatest.diagrams.Diagrams.assume(org.exists(_ == 'b'), "this is a clue")
          """.stripMargin)
      }

      it("should do nothing when is used to check new String(\"test\") != \"test\"") {
        org.scalatest.diagrams.Diagrams.assume(new String("test") == "test", "this is a clue")
      }

      it("should throw TestCanceledException with correct message and stack depth when is used to check new String(\"test\") != \"testing\"") {
        val e = intercept[TestCanceledException] {
          org.scalatest.diagrams.Diagrams.assume(new String("test") == "testing", "this is a clue")
        }
        e.message should be (
          Some(
            """this is a clue
              |
              |org.scalatest.diagrams.Diagrams.assume(new String("test") == "testing", "this is a clue")
              |                                       |                  |  |
              |                                       "test"             |  "testing"
              |                                                          false
              |""".stripMargin
          )
        )
        e.failedCodeFileName should be (Some(fileName))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 14))
      }

      it("should compile when used with Java static method") {
        assertCompiles(
          """
            |org.scalatest.diagrams.Diagrams.assume(System.currentTimeMillis() > 0, "this is a clue")
          """.stripMargin)
        assertCompiles(
          """
            |org.scalatest.diagrams.Diagrams.assume(java.math.BigDecimal.ZERO == java.math.BigDecimal.ZERO, "this is a clue")
          """.stripMargin)
      }
    }

  }

}

