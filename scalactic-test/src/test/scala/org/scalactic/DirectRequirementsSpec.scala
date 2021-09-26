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
package org.scalactic

import org.scalatest.{FailureMessages => _, UnquotedString => _, _}
import java.util.Date
import Prettifier.lineSeparator
import org.scalactic.exceptions.NullArgumentException

class DirectRequirementsSpec extends FunSpec with OptionValues {

  private def neverRuns1(f: => Unit): Boolean = true
  private def neverRuns2(f: => Unit)(a: Int): Boolean = true
  private def neverRuns3[T](f: => Unit)(a: T): Boolean = true

  private val prettifier = Prettifier.default

  def didNotEqual(left: Any, right: Any): String = {
    val (leftee, rightee) = Prettifier.getObjectsForFailureMessage(left, right)
    FailureMessages.didNotEqual(prettifier, leftee, rightee)
  }

  def equaled(left: Any, right: Any): String =
    FailureMessages.equaled(prettifier, left, right)

  def wasNotGreaterThan(left: Any, right: Any): String =
    FailureMessages.wasNotGreaterThan(prettifier, left, right)

  def wasGreaterThan(left: Any, right: Any): String =
    FailureMessages.wasGreaterThan(prettifier, left, right)

  def wasNotGreaterThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages.wasNotGreaterThanOrEqualTo(prettifier, left, right)

  def wasGreaterThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages.wasGreaterThanOrEqualTo(prettifier, left, right)

  def wasNotLessThan(left: Any, right: Any): String =
    FailureMessages.wasNotLessThan(prettifier, left, right)

  def wasLessThan(left: Any, right: Any): String =
    FailureMessages.wasLessThan(prettifier, left, right)

  def wasNotLessThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages.wasNotLessThanOrEqualTo(prettifier, left, right)

  def wasLessThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages.wasLessThanOrEqualTo(prettifier, left, right)

  def commaAnd(left: String, right: String): String =
  // SKIP-DOTTY-START
    FailureMessages.commaAnd(prettifier, UnquotedString(left), UnquotedString(right))
  // SKIP-DOTTY-END
  //DOTTY-ONLY FailureMessages.commaAnd(prettifier, left, right)

  def commaBut(left: String, right: String): String =
  // SKIP-DOTTY-START
    FailureMessages.commaBut(prettifier, UnquotedString(left), UnquotedString(right))
  // SKIP-DOTTY-END
  //DOTTY-ONLY FailureMessages.commaBut(prettifier, left, right)

  def wasFalse(left: String): String =
    left + " was false"

  def wasTrue(left: String): String =
    left + " was true"

  def didNotStartWith(left: Any, right: Any): String =
    FailureMessages.didNotStartWith(prettifier, left, right)

  def startedWith(left: Any, right: Any): String =
    FailureMessages.startedWith(prettifier, left, right)

  def didNotEndWith(left: Any, right: Any): String =
    FailureMessages.didNotEndWith(prettifier, left, right)

  def endedWith(left: Any, right: Any): String =
    FailureMessages.endedWith(prettifier, left, right)

  def didNotContain(left: Any, right: Any): String =
    FailureMessages.didNotContain(prettifier, left, right)

  def contained(left: Any, right: Any): String =
    FailureMessages.contained(prettifier, left, right)

  def didNotContainKey(left: Any, right: Any): String =
    FailureMessages.didNotContainKey(prettifier, left, right)

  def containedKey(left: Any, right: Any): String =
    FailureMessages.containedKey(prettifier, left, right)

  def wasNotTheSameInstanceAs(left: AnyRef, right: AnyRef): String =
    FailureMessages.wasNotTheSameInstanceAs(prettifier, left, right)

  def wasTheSameInstanceAs(left: AnyRef, right: AnyRef): String =
    FailureMessages.wasTheSameInstanceAs(prettifier, left, right)

  def wasNotEmpty(left: Any): String =
    FailureMessages.wasNotEmpty(prettifier, left)

  def wasEmpty(left: Any): String =
    FailureMessages.wasEmpty(prettifier, left)

  def wasNotInstanceOf(left: Any, className: String): String =
    FailureMessages.wasNotInstanceOf(prettifier, left, UnquotedString(className))

  def wasInstanceOf(left: Any, className: String): String =
    FailureMessages.wasInstanceOf(prettifier, left, UnquotedString(className))

  def hadLengthInsteadOfExpectedLength(left: Any, actual: Any, expected: Any): String =
    FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, left, actual, expected)

  def hadLength(left: Any, actual: Long): String =
    FailureMessages.hadLength(prettifier, left, actual)

  def hadSizeInsteadOfExpectedSize(left: Any, actual: Any, expected: Any): String =
    FailureMessages.hadSizeInsteadOfExpectedSize(prettifier, left, actual, expected)

  def hadSize(left: Any, actual: Long): String =
    FailureMessages.hadSize(prettifier, left, actual)

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

  class FloatLengthSize(value: Float) {
    val length: Float = value
    val size: Float = value
  }

  val floatLengthSize = new FloatLengthSize(2.0f)

  describe("The require(boolean) method") {

    val a = 3
    val b = 5

    it("should do nothing when is used to check a == 3") {
      org.scalactic.Requirements.require(a == 3)
    }

    it("should throw IllegalArgumentException when is used to check a == 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 5 == b") {
      org.scalactic.Requirements.require(5 == b)
    }

    it("should throw IllegalArgumentException when is used to check 3 == b") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(3 == b)
      }
      assert(e.getMessage == didNotEqual(3, b))
    }

    it("should do nothing when is used to check 3 == 3") {
      org.scalactic.Requirements.require(3 == 3)
    }

    it("should throw IllegalArgumentException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(3 == 5)
      }
      assert(e.getMessage == "Expression was false")
    }

    it("should throw IllegalArgumentException when is used to check a == b") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == b)
      }
      assert(e.getMessage == didNotEqual(a, b))
    }

    it("should throw IllegalArgumentException when is used to check a == -1") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == -1)
      }
      assert(e.getMessage == didNotEqual(a, -1))
    }

    it("should throw IllegalArgumentException when is used to check -1 == a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(-1 == a)
      }
      assert(e.getMessage == didNotEqual(-1, a))
    }

    it("should do nothing when is used to check a === 3") {
      org.scalactic.Requirements.require(a === 3)
    }

    it("should throw IllegalArgumentException when is used to check a === 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a === 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 3 === a") {
      org.scalactic.Requirements.require(3 === a)
    }

    it("should throw IllegalArgumentException when is used to check 5 === a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(5 === a)
      }
      assert(e.getMessage == didNotEqual(5, a))
    }

    it("should do nothing when is used to check a !== 5") {
      org.scalactic.Requirements.require(a !== 5)
    }

    it("should throw IllegalArgumentException when is used to check a !== 3") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a !== 3)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should do nothing when is used to check 5 !== a") {
      org.scalactic.Requirements.require(5 !== a)
    }

    it("should throw IllegalArgumentException when is used to check 3 !== a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(3 !== a)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should do nothing when is used to check a > 2") {
      org.scalactic.Requirements.require(a > 2)
    }

    it("should do nothing when is used to check 5 > a") {
      org.scalactic.Requirements.require(5 > a)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a > 3)
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(3 > a)
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3))
    }

    it("should do nothing when is used to check a >= 3") {
      org.scalactic.Requirements.require(a >= 3)
    }

    it("should do nothing when is used to check 3 >= a") {
      org.scalactic.Requirements.require(3 >= a)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a >= 4)
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(3, 4))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(2 >= a)
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(2, 3))
    }

    it("should do nothing when is used to check b < 6") {
      org.scalactic.Requirements.require(b < 6)
    }

    it("should do nothing when is used to check 3 < b") {
      org.scalactic.Requirements.require(3 < b)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(b < 5)
      }
      assert(e.getMessage == wasNotLessThan(5, 5))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(5 < b)
      }
      assert(e.getMessage == wasNotLessThan(5, 5))
    }

    it("should do nothing when is used to check b <= 5") {
      org.scalactic.Requirements.require(b <= 5)
    }

    it("should do nothing when is used to check 5 <= b") {
      org.scalactic.Requirements.require(5 <= b)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(b <= 4)
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(5, 4))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(6 <= b)
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(6, 5))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      org.scalactic.Requirements.require(a == 3 && b == 5)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 3 && b == 6)
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 && b == 5)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 && b == 6)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      org.scalactic.Requirements.require(a == 3 & b == 5)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 3 & b == 6)
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 & b == 5)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 & b == 6)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      org.scalactic.Requirements.require(a == 3 || b == 5)
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      org.scalactic.Requirements.require(a == 3 || b == 6)
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      org.scalactic.Requirements.require(a == 2 || b == 5)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 || b == 6)
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      org.scalactic.Requirements.require(a == 3 | b == 5)
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      org.scalactic.Requirements.require(a == 3 | b == 6)
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      org.scalactic.Requirements.require(a == 2 | b == 5)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 | b == 6)
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      org.scalactic.Requirements.require(a == 3 && (b == 5 && b > 3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 3 && (b == 5 && b > 5))
      }
      assert(e.getMessage == commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))))
    }

    it("should do nothing when is used to check !(a == 5)") {
      org.scalactic.Requirements.require(!(a == 5))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(a == 3))
      }
      assert(e.getMessage == equaled(3, 3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 3 && !(b == 5))
      }
      assert(e.getMessage == commaBut(equaled(3, 3), equaled(5, 5)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      org.scalactic.Requirements.require((a == 3) == (b == 5))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require((a == 3) == (b != 5))
      }
      assert(e.getMessage === didNotEqual(true, false))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 5 && s.changeState)
      }
      assert(s.state == false)
    }

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 5 & s.changeState)
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      org.scalactic.Requirements.require(a == 3 || s.changeState)
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      org.scalactic.Requirements.require(a == 3 | s.changeState)
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      org.scalactic.Requirements.require(a == 3 && { println("hi"); b == 5})
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 3 && { println("hi"); b == 3})
      }
      if (ScalacticVersions.BuiltForScalaVersion == "2.12" || ScalacticVersions.BuiltForScalaVersion == "2.13")
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.Predef.println(\"hi\");" + lineSeparator + "  b.==(3)" + lineSeparator + "}")))
      else if (ScalacticVersions.BuiltForScalaVersion.startsWith("0."))
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.Predef.println(\"hi\")" + lineSeparator + "  b.==(3)" + lineSeparator + "}")))
      else
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.this.Predef.println(\"hi\");" + lineSeparator + "  b.==(3)" + lineSeparator + "}")))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      org.scalactic.Requirements.require({ println("hi"); b == 5} && a == 3)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require({ println("hi"); b == 5} && a == 5)
      }
      if (ScalacticVersions.BuiltForScalaVersion == "2.12" || ScalacticVersions.BuiltForScalaVersion == "2.13")
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.Predef.println(\"hi\");" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)))
      else if (ScalacticVersions.BuiltForScalaVersion.startsWith("0."))
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.Predef.println(\"hi\")" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)))
      else
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.this.Predef.println(\"hi\");" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      org.scalactic.Requirements.require(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      org.scalactic.Requirements.require(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      org.scalactic.Requirements.require(neverRuns3(sys.error("Sad times 3"))(0))
    }

    val s1 = "hi ScalaTest"
    val s2 = "ScalaTest hi"
    val s3 = "Say hi to ScalaTest"
    val s4 = ""

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)
    val l2 = List.empty[Int]
    val l3 = List("one", "two", "three")

    val m1 = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val m2 = Map.empty[Int, String]

    val ct1 = new CustomContainer(8)

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      org.scalactic.Requirements.require(s1 startsWith "hi")
      org.scalactic.Requirements.require(s1.startsWith("hi"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s2 startsWith "hi")
      }
      assert(e1.getMessage == didNotStartWith(s2, "hi"))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s2.startsWith("hi"))
      }
      assert(e2.getMessage == didNotStartWith(s2, "hi"))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      org.scalactic.Requirements.require(ci1 startsWith 1)
      org.scalactic.Requirements.require(ci1.startsWith(1))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci2 startsWith 1)
      }
      assert(e1.getMessage == didNotStartWith(ci2, 1))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci2.startsWith(1))
      }
      assert(e2.getMessage == didNotStartWith(ci2, 1))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      org.scalactic.Requirements.require(!s2.startsWith("hi"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s1.startsWith("hi"))
      }
      assert(e1.getMessage == startedWith(s1, "hi"))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      org.scalactic.Requirements.require(s2 endsWith "hi")
      org.scalactic.Requirements.require(s2.endsWith("hi"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s1 endsWith "hi")
      }
      assert(e1.getMessage == didNotEndWith(s1, "hi"))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s1.endsWith("hi"))
      }
      assert(e2.getMessage == didNotEndWith(s1, "hi"))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      org.scalactic.Requirements.require(ci2 endsWith 1)
      org.scalactic.Requirements.require(ci2.endsWith(1))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1 endsWith 1)
      }
      assert(e1.getMessage == didNotEndWith(ci1, 1))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1.endsWith(1))
      }
      assert(e2.getMessage == didNotEndWith(ci1, 1))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      org.scalactic.Requirements.require(!s1.endsWith("hi"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s2.endsWith("hi"))
      }
      assert(e1.getMessage == endedWith(s2, "hi"))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      org.scalactic.Requirements.require(s3 contains "hi")
      org.scalactic.Requirements.require(s3.contains("hi"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s3 contains "hello")
      }
      assert(e1.getMessage == didNotContain(s3, "hello"))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s3.contains("hello"))
      }
      assert(e2.getMessage == didNotContain(s3, "hello"))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      org.scalactic.Requirements.require(ci2 contains 2)
      org.scalactic.Requirements.require(ci2.contains(2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1 contains 5)
      }
      assert(e1.getMessage == didNotContain(ci1, 5))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1.contains(5))
      }
      assert(e2.getMessage == didNotContain(ci1, 5))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      org.scalactic.Requirements.require(!s3.contains("hello"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s3.contains("hi"))
      }
      assert(e1.getMessage == contained(s3, "hi"))
    }

    it("should do nothing when is used to check l1 contains 2") {
      org.scalactic.Requirements.require(l1 contains 2)
      org.scalactic.Requirements.require(l1.contains(2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1 contains 5)
      }
      assert(e1.getMessage == didNotContain(l1, 5))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.contains(5))
      }
      assert(e2.getMessage == didNotContain(l1, 5))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      org.scalactic.Requirements.require(!(l1 contains 5))
      org.scalactic.Requirements.require(!l1.contains(5))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(l1 contains 2))
      }
      assert(e1.getMessage == contained(l1, 2))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!l1.contains(2))
      }
      assert(e2.getMessage == contained(l1, 2))
    }

    it("should do nothing when is used to check m1 contains 2") {
      org.scalactic.Requirements.require(m1 contains 2)
      org.scalactic.Requirements.require(m1.contains(2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(m1 contains 5)
      }
      assert(e1.getMessage == didNotContainKey(m1, 5))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(m1.contains(5))
      }
      assert(e2.getMessage == didNotContainKey(m1, 5))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      org.scalactic.Requirements.require(!(m1 contains 5))
      org.scalactic.Requirements.require(!m1.contains(5))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(m1 contains 2))
      }
      assert(e1.getMessage == containedKey(m1, 2))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!m1.contains(2))
      }
      assert(e2.getMessage == containedKey(m1, 2))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      org.scalactic.Requirements.require(ct1 contains 8)
      org.scalactic.Requirements.require(ct1.contains(8))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ct1 contains 5)
      }
      assert(e1.getMessage == didNotContain(ct1, 5))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ct1.contains(5))
      }
      assert(e2.getMessage == didNotContain(ct1, 5))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      org.scalactic.Requirements.require(!ct1.contains(5))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!ct1.contains(8))
      }
      assert(e1.getMessage == contained(ct1, 8))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      org.scalactic.Requirements.require(ci1 eq ci3)
      org.scalactic.Requirements.require(ci1.eq(ci3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1 eq ci2)
      }
      assert(e1.getMessage == wasNotTheSameInstanceAs(ci1, ci2))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1.eq(ci2))
      }
      assert(e2.getMessage == wasNotTheSameInstanceAs(ci1, ci2))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      org.scalactic.Requirements.require(!ci1.eq(ci2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!ci1.eq(ci3))
      }
      assert(e.getMessage == wasTheSameInstanceAs(ci1, ci3))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      org.scalactic.Requirements.require(ci1 ne ci2)
      org.scalactic.Requirements.require(ci1.ne(ci2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1 ne ci3)
      }
      assert(e1.getMessage == wasTheSameInstanceAs(ci1, ci3))

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1.ne(ci3))
      }
      assert(e2.getMessage == wasTheSameInstanceAs(ci1, ci3))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      org.scalactic.Requirements.require(!ci1.ne(ci3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!ci1.ne(ci2))
      }
      assert(e.getMessage == wasNotTheSameInstanceAs(ci1, ci2))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      org.scalactic.Requirements.require(s4.isEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s3.isEmpty)
      }
      assert(e.getMessage == wasNotEmpty(s3))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      org.scalactic.Requirements.require(!s3.isEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s4.isEmpty)
      }
      assert(e.getMessage == wasEmpty(s4))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      org.scalactic.Requirements.require(l2.isEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.isEmpty)
      }
      assert(e.getMessage == wasNotEmpty(l1))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      org.scalactic.Requirements.require(!l1.isEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!l2.isEmpty)
      }
      assert(e.getMessage == wasEmpty(l2))
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      org.scalactic.Requirements.require(s3.nonEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s4.nonEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s4.nonEmpty)
      }
      assert(e.getMessage == wasEmpty(s4))
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      org.scalactic.Requirements.require(!s4.nonEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s3.nonEmpty)
      }
      assert(e.getMessage == wasNotEmpty(s3))
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      org.scalactic.Requirements.require(l1.nonEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l2.nonEmpty)
      }
      assert(e.getMessage == wasEmpty(l2))
    }

    it("should do nothing when is used to check !l2.nonEmpty") {
      org.scalactic.Requirements.require(!l2.nonEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!l1.nonEmpty)
      }
      assert(e.getMessage == wasNotEmpty(l1))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      org.scalactic.Requirements.require(s1.isInstanceOf[String])
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.isInstanceOf[String])
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "scala.Predef.String"))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      org.scalactic.Requirements.require(l1.isInstanceOf[List[Int]])
    }

    // SKIP-DOTTY-START
    // different printing of `List[Int]`
    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s1.isInstanceOf[List[Int]])
      }
      assert(e.getMessage == wasNotInstanceOf(s1, "scala.List"))
    }
    // SKIP-DOTTY-END

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      org.scalactic.Requirements.require(date.isInstanceOf[Date])
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.isInstanceOf[Date])
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "java.util.Date"))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      org.scalactic.Requirements.require(!l1.isInstanceOf[String])
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s1.isInstanceOf[String])
      }
      assert(e.getMessage == wasInstanceOf(s1, "scala.Predef.String"))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      org.scalactic.Requirements.require(!s1.isInstanceOf[List[Int]])
    }

    // SKIP-DOTTY-START
    // different printing of `List[Int]`
    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!l1.isInstanceOf[List[Int]])
      }
      assert(e.getMessage == wasInstanceOf(l1, "scala.List"))
    }
    // SKIP-DOTTY-END

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      org.scalactic.Requirements.require(!l1.isInstanceOf[Date])
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!date.isInstanceOf[Date])
      }
      assert(e.getMessage == wasInstanceOf(date, "java.util.Date"))
    }

    it("should do nothing when is used to check s1.length == 9") {
      org.scalactic.Requirements.require(s1.length == 12)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s1.length == 10)
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(s1, 12, 10))
    }

    it("should do nothing when is used to check l1.length == 3") {
      org.scalactic.Requirements.require(l1.length == 3)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.length == 10)
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(l1, 3, 10))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      org.scalactic.Requirements.require(!(s1.length == 10))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(s1.length == 12))
      }
      assert(e.getMessage == hadLength(s1, 12))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      org.scalactic.Requirements.require(!(l1.length == 2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(l1.length == 3))
      }
      assert(e.getMessage == hadLength(l1, 3))
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      org.scalactic.Requirements.require(floatLengthSize.length == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(floatLengthSize.length == 1.0f)
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f))
    }

    it("should do nothing when is used to check s1.size == 9") {
      org.scalactic.Requirements.require(s1.size == 12)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s1.size == 10)
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(s1, 12, 10))
    }

    it("should do nothing when is used to check l1.size == 3") {
      org.scalactic.Requirements.require(l1.size == 3)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.size == 10)
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(l1, 3, 10))
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      org.scalactic.Requirements.require(!(s1.size == 10))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(s1.size == 12))
      }
      assert(e.getMessage == hadSize(s1, 12))
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      org.scalactic.Requirements.require(!(l1.size == 2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(l1.size == 3))
      }
      assert(e.getMessage == hadSize(l1, 3))
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      org.scalactic.Requirements.require(floatLengthSize.size == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(floatLengthSize.size == 1.0f)
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f))
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      org.scalactic.Requirements.require(l1.exists(_ == 3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.exists(_ == 5))
      }
      assert(e.getMessage == didNotContain(l1, 5))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      org.scalactic.Requirements.require(!l1.exists(_ == 5))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!l1.exists(_ == 3))
      }
      assert(e.getMessage == contained(l1, 3))
    }

    // SKIP-DOTTY-START
    // different printing of anonymous functions
    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.exists(_ > 3))
      }
      assert(e.getMessage == wasFalse("l1.exists(((x$6: Int) => x$6.>(3)))"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l3.exists(_.isEmpty))
      }
      assert(e.getMessage == wasFalse("l3.exists(((x$7: String) => x$7.isEmpty()))"))
    }
    // SKIP-DOTTY-END

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1.exists(321))
      }
      assert(e.getMessage == wasFalse("ci1.exists(321)"))
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      org.scalactic.Requirements.require(woof { meow(y = 5) } == "woof")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(woof { meow(y = 5) } == "meow")
      }
      assert(e.getMessage == didNotEqual("woof", "meow"))
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalactic.Requirements.require(org == "test")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalactic.Requirements.require(org === "test")
        """.stripMargin)
    }

    // SKIP-DOTTY-START
    // ambiguous implicit:
    //   both method convertToCheckingEqualizer in trait TypeCheckedTripleEquals and method convertToEqualizer in trait TripleEquals provide an extension method `===` on String(org)
    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends FunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    _root_.org.scalactic.Requirements.require(org === "test")
          |  }
          |}
        """.stripMargin)
    }
    // SKIP-DOTTY-END

    it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
      assertCompiles(
        """
          |class Test {
          |  def aCustomMethod: Boolean = true
          |}
          |val org = new Test
          |_root_.org.scalactic.Requirements.require(org.aCustomMethod)
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |_root_.org.scalactic.Requirements.require(!org)
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.require(org.isEmpty)
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.require(org.isInstanceOf[String])
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |_root_.org.scalactic.Requirements.require(org.size == 0)
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.require(org.length == 0)
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |_root_.org.scalactic.Requirements.require(org.exists(_ == 'b'))
        """.stripMargin)
    }

  }

  describe("The require(boolean, clue) method") {

    val a = 3
    val b = 5

    it("should throw NullPointerException when null is passed in as clue") {
      val e = intercept[NullPointerException] {
        org.scalactic.Requirements.require(a == 3, null)
      }
      assert(e.getMessage == "clue was null")
    }

    it("should do nothing when is used to check a == 3") {
      org.scalactic.Requirements.require(a == 3, "dude")
    }

    it("should throw IllegalArgumentException when is used to check a == 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 5 == b") {
      org.scalactic.Requirements.require(5 == b, ", dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 == b") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(3 == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, b) + ", dude")
    }

    it("should do nothing when is used to check a != 5") {
      org.scalactic.Requirements.require(a != 5, ". dude")
    }

    it("should throw IllegalArgumentException when is used to check a != 3") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a != 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 3 != b") {
      org.scalactic.Requirements.require(3 != b, "; dude")
    }

    it("should throw IllegalArgumentException when is used to check 5 != b") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(5 != b, "; dude")
      }
      assert(e.getMessage == equaled(5, b) + "; dude")
    }

    it("should do nothing when is used to check 3 == 3") {
      org.scalactic.Requirements.require(3 == 3, "dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(3 == 5, "dude")
      }
      assert(e.getMessage == "dude")
    }

    it("should throw IllegalArgumentException when is used to check a == b") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(a, b) + ", dude")
    }

    it("should throw IllegalArgumentException when is used to check a == -1") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == -1, ". dude")
      }
      assert(e.getMessage == didNotEqual(a, -1) + ". dude")
    }

    it("should throw IllegalArgumentException when is used to check -1 == a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(-1 == a, "; dude")
      }
      assert(e.getMessage == didNotEqual(-1, a) + "; dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 != a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(3 != a, ", dude")
      }
      assert(e.getMessage == equaled(3, a) + ", dude")
    }

    it("should do nothing when is used to check 5 != a") {
      org.scalactic.Requirements.require(5 != a, ". dude")
    }

    it("should do nothing when is used to check a === 3") {
      org.scalactic.Requirements.require(a === 3, "dude")
    }

    it("should throw IllegalArgumentException when is used to check a === 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a === 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 3 === a") {
      org.scalactic.Requirements.require(3 === a, ", dude")
    }

    it("should throw IllegalArgumentException when is used to check 5 === a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(5 === a, ", dude")
      }
      assert(e.getMessage == didNotEqual(5, a) + ", dude")
    }

    it("should do nothing when is used to check a !== 5") {
      org.scalactic.Requirements.require(a !== 5, ". dude")
    }

    it("should throw IllegalArgumentException when is used to check a !== 3") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a !== 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 5 !== a") {
      org.scalactic.Requirements.require(5 !== a, "; dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 !== a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(3 !== a, "; dude")
      }
      assert(e.getMessage == equaled(3, a) + "; dude")
    }

    it("should do nothing when is used to check a > 2") {
      org.scalactic.Requirements.require(a > 2, ", dude")
    }

    it("should do nothing when is used to check 5 > a") {
      org.scalactic.Requirements.require(5 > a, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a > 3, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(3 > a, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3) + ", dude")
    }

    it("should do nothing when is used to check a >= 3") {
      org.scalactic.Requirements.require(a >= 3, ", dude")
    }

    it("should do nothing when is used to check 3 >= a") {
      org.scalactic.Requirements.require(3 >= a, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a >= 4, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(3, 4) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(2 >= a, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(2, 3) + ", dude")
    }

    it("should do nothing when is used to check b < 6") {
      org.scalactic.Requirements.require(b < 6, ", dude")
    }

    it("should do nothing when is used to check 3 < b") {
      org.scalactic.Requirements.require(3 < b, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(b < 5, ", dude")
      }
      assert(e.getMessage == wasNotLessThan(5, 5) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(5 < b, ", dude")
      }
      assert(e.getMessage == wasNotLessThan(5, 5) + ", dude")
    }

    it("should do nothing when is used to check b <= 5") {
      org.scalactic.Requirements.require(b <= 5, ", dude")
    }

    it("should do nothing when is used to check 5 <= b") {
      org.scalactic.Requirements.require(5 <= b, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(b <= 4, ", dude")
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(5, 4) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(6 <= b, ", dude")
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(6, 5) + ", dude")
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      org.scalactic.Requirements.require(a == 3 && b == 5, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 3 && b == 6, ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 && b == 5, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 && b == 6, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      org.scalactic.Requirements.require(a == 3 & b == 5, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 3 & b == 6, ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 & b == 5, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 & b == 6, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      org.scalactic.Requirements.require(a == 3 || b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      org.scalactic.Requirements.require(a == 3 || b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      org.scalactic.Requirements.require(a == 2 || b == 5, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 || b == 6, ", dude")
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      org.scalactic.Requirements.require(a == 3 | b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      org.scalactic.Requirements.require(a == 3 | b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      org.scalactic.Requirements.require(a == 2 | b == 5, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 2 | b == 6, ", dude")
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude")
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      org.scalactic.Requirements.require(a == 3 && (b == 5 && b > 3), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 3 && (b == 5 && b > 5), ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))) + ", dude")
    }

    it("should do nothing when is used to check !(a == 5)") {
      org.scalactic.Requirements.require(!(a == 5), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(a == 3), ", dude")
      }
      assert(e.getMessage == equaled(3, 3) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 3 && !(b == 5), ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), equaled(5, 5)) + ", dude")
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      org.scalactic.Requirements.require((a == 3) == (b == 5), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require((a == 3) == (b != 5), ", dude")
      }
      assert(e.getMessage === didNotEqual(true, false) + ", dude")
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 5 && s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 5 & s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      org.scalactic.Requirements.require(a == 3 || s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      org.scalactic.Requirements.require(a == 3 | s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      org.scalactic.Requirements.require(a == 3 && { println("hi"); b == 5}, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(a == 3 && { println("hi"); b == 3}, ", dude")
      }
      if (ScalacticVersions.BuiltForScalaVersion == "2.12" || ScalacticVersions.BuiltForScalaVersion == "2.13")
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.Predef.println(\"hi\");" + lineSeparator + "  b.==(3)" + lineSeparator + "}")) + ", dude")
      else if (ScalacticVersions.BuiltForScalaVersion.startsWith("0."))
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.Predef.println(\"hi\")" + lineSeparator + "  b.==(3)" + lineSeparator + "}")) + ", dude")
      else
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.this.Predef.println(\"hi\");" + lineSeparator + "  b.==(3)" + lineSeparator + "}")) + ", dude")
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      org.scalactic.Requirements.require({ println("hi"); b == 5} && a == 3, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require({ println("hi"); b == 5} && a == 5, ", dude")
      }
      if (ScalacticVersions.BuiltForScalaVersion == "2.12" || ScalacticVersions.BuiltForScalaVersion == "2.13")
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.Predef.println(\"hi\");" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)) + ", dude")
      else if (ScalacticVersions.BuiltForScalaVersion.startsWith("0."))
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.Predef.println(\"hi\")" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)) + ", dude")
      else
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.this.Predef.println(\"hi\");" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)) + ", dude")
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      org.scalactic.Requirements.require(neverRuns1(sys.error("Sad times 1")), "should not fail!")
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      org.scalactic.Requirements.require(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      org.scalactic.Requirements.require(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
    }

    val s1 = "hi ScalaTest"
    val s2 = "ScalaTest hi"
    val s3 = "Say hi to ScalaTest"
    val s4 = ""

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)
    val l2 = List.empty[Int]
    val l3 = List("one", "two", "three")

    val m1 = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val m2 = Map.empty[Int, String]

    val ct1 = new CustomContainer(8)

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      org.scalactic.Requirements.require(s1 startsWith "hi", ", dude")
      org.scalactic.Requirements.require(s1.startsWith("hi"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s2 startsWith "hi", ", dude")
      }
      assert(e1.getMessage == didNotStartWith(s2, "hi") + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s2.startsWith("hi"), ", dude")
      }
      assert(e2.getMessage == didNotStartWith(s2, "hi") + ", dude")
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      org.scalactic.Requirements.require(ci1 startsWith 1, ", dude")
      org.scalactic.Requirements.require(ci1.startsWith(1), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci2 startsWith 1, ", dude")
      }
      assert(e1.getMessage == didNotStartWith(ci2, 1) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci2.startsWith(1), ", dude")
      }
      assert(e2.getMessage == didNotStartWith(ci2, 1) + ", dude")
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      org.scalactic.Requirements.require(!s2.startsWith("hi"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s1.startsWith("hi"), ", dude")
      }
      assert(e1.getMessage == startedWith(s1, "hi") + ", dude")
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      org.scalactic.Requirements.require(s2 endsWith "hi", ", dude")
      org.scalactic.Requirements.require(s2.endsWith("hi"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s1 endsWith "hi", ", dude")
      }
      assert(e1.getMessage == didNotEndWith(s1, "hi") + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s1.endsWith("hi"), ", dude")
      }
      assert(e2.getMessage == didNotEndWith(s1, "hi") + ", dude")
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      org.scalactic.Requirements.require(ci2 endsWith 1, ", dude")
      org.scalactic.Requirements.require(ci2.endsWith(1), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1 endsWith 1, ", dude")
      }
      assert(e1.getMessage == didNotEndWith(ci1, 1) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1.endsWith(1), ", dude")
      }
      assert(e2.getMessage == didNotEndWith(ci1, 1) + ", dude")
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      org.scalactic.Requirements.require(!s1.endsWith("hi"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s2.endsWith("hi"), ", dude")
      }
      assert(e1.getMessage == endedWith(s2, "hi") + ", dude")
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      org.scalactic.Requirements.require(s3 contains "hi", ", dude")
      org.scalactic.Requirements.require(s3.contains("hi"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s3 contains "hello", ", dude")
      }
      assert(e1.getMessage == didNotContain(s3, "hello") + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s3.contains("hello"), ", dude")
      }
      assert(e2.getMessage == didNotContain(s3, "hello") + ", dude")
    }

    it("should do nothing when is used to check ci2 contains 2") {
      org.scalactic.Requirements.require(ci2 contains 2, ", dude")
      org.scalactic.Requirements.require(ci2.contains(2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContain(ci1, 5) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContain(ci1, 5) + ", dude")
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      org.scalactic.Requirements.require(!s3.contains("hello"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s3.contains("hi"), ", dude")
      }
      assert(e1.getMessage == contained(s3, "hi") + ", dude")
    }

    it("should do nothing when is used to check l1 contains 2") {
      org.scalactic.Requirements.require(l1 contains 2, ", dude")
      org.scalactic.Requirements.require(l1.contains(2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContain(l1, 5) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContain(l1, 5) + ", dude")
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      org.scalactic.Requirements.require(!(l1 contains 5), ", dude")
      org.scalactic.Requirements.require(!l1.contains(5), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(l1 contains 2), ", dude")
      }
      assert(e1.getMessage == contained(l1, 2) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!l1.contains(2), ", dude")
      }
      assert(e2.getMessage == contained(l1, 2) + ", dude")
    }

    it("should do nothing when is used to check m1 contains 2") {
      org.scalactic.Requirements.require(m1 contains 2, ", dude")
      org.scalactic.Requirements.require(m1.contains(2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(m1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContainKey(m1, 5) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(m1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContainKey(m1, 5) + ", dude")
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      org.scalactic.Requirements.require(!(m1 contains 5), ", dude")
      org.scalactic.Requirements.require(!m1.contains(5), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(m1 contains 2), ", dude")
      }
      assert(e1.getMessage == containedKey(m1, 2) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!m1.contains(2), ", dude")
      }
      assert(e2.getMessage == containedKey(m1, 2) + ", dude")
    }

    it("should do nothing when is used to check ct1 contains 8") {
      org.scalactic.Requirements.require(ct1 contains 8, ", dude")
      org.scalactic.Requirements.require(ct1.contains(8), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ct1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContain(ct1, 5) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ct1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContain(ct1, 5) + ", dude")
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      org.scalactic.Requirements.require(!ct1.contains(5), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!ct1.contains(8), ", dude")
      }
      assert(e1.getMessage == contained(ct1, 8) + ", dude")
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      org.scalactic.Requirements.require(ci1 eq ci3, ", dude")
      org.scalactic.Requirements.require(ci1.eq(ci3), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1 eq ci2, ", dude")
      }
      assert(e1.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1.eq(ci2), ", dude")
      }
      assert(e2.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      org.scalactic.Requirements.require(!ci1.eq(ci2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!ci1.eq(ci3), ", dude")
      }
      assert(e.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      org.scalactic.Requirements.require(ci1 ne ci2, ", dude")
      org.scalactic.Requirements.require(ci1.ne(ci2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1 ne ci3, ", dude")
      }
      assert(e1.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1.ne(ci3), ", dude")
      }
      assert(e2.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      org.scalactic.Requirements.require(!ci1.ne(ci3), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!ci1.ne(ci2), ", dude")
      }
      assert(e.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")
    }

    it("should do nothing when is used to check s4.isEmpty") {
      org.scalactic.Requirements.require(s4.isEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s3.isEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(s3) + ", dude")
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      org.scalactic.Requirements.require(!s3.isEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s4.isEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(s4) + ", dude")
    }

    it("should do nothing when is used to check l2.isEmpty") {
      org.scalactic.Requirements.require(l2.isEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.isEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(l1) + ", dude")
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      org.scalactic.Requirements.require(!l1.isEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!l2.isEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(l2) + ", dude")
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      org.scalactic.Requirements.require(s3.nonEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s4.nonEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s4.nonEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(s4) + ", dude")
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      org.scalactic.Requirements.require(!s4.nonEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s3.nonEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(s3) + ", dude")
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      org.scalactic.Requirements.require(l1.nonEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l2.nonEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(l2) + ", dude")
    }

    it("should do nothing when is used to check !l2.nonEmpty") {
      org.scalactic.Requirements.require(!l2.nonEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!l1.nonEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(l1) + ", dude")
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      org.scalactic.Requirements.require(s1.isInstanceOf[String], ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.isInstanceOf[String], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "scala.Predef.String") + ", dude")
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      org.scalactic.Requirements.require(l1.isInstanceOf[List[Int]], ", dude")
    }

    // SKIP-DOTTY-START
    // different printing of `List[Int]`
    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(s1, "scala.List") + ", dude")
    }
    // SKIP-DOTTY-END

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      org.scalactic.Requirements.require(date.isInstanceOf[Date], ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.isInstanceOf[Date], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "java.util.Date") + ", dude")
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      org.scalactic.Requirements.require(!l1.isInstanceOf[String], ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!s1.isInstanceOf[String], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(s1, "scala.Predef.String") + ", dude")
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      org.scalactic.Requirements.require(!s1.isInstanceOf[List[Int]], ", dude")
    }

    // SKIP-DOTTY-START
    // different printing of `List[Int]`
    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!l1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(l1, "scala.List") + ", dude")
    }
    // SKIP-DOTTY-END

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      org.scalactic.Requirements.require(!l1.isInstanceOf[Date], ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!date.isInstanceOf[Date], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(date, "java.util.Date") + ", dude")
    }

    it("should do nothing when is used to check s1.length == 9") {
      org.scalactic.Requirements.require(s1.length == 12, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s1.length == 10, ", dude")
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(s1, 12, 10) + ", dude")
    }

    it("should do nothing when is used to check l1.length == 3") {
      org.scalactic.Requirements.require(l1.length == 3, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.length == 10, ", dude")
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(l1, 3, 10) + ", dude")
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      org.scalactic.Requirements.require(!(s1.length == 10), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(s1.length == 12), ", dude")
      }
      assert(e.getMessage == hadLength(s1, 12) + ", dude")
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      org.scalactic.Requirements.require(!(l1.length == 2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(l1.length == 3), ", dude")
      }
      assert(e.getMessage == hadLength(l1, 3) + ", dude")
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      org.scalactic.Requirements.require(floatLengthSize.length == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(floatLengthSize.length == 1.0f, ", dude")
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f) + ", dude")
    }

    it("should do nothing when is used to check s1.size == 9") {
      org.scalactic.Requirements.require(s1.size == 12, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(s1.size == 10, ", dude")
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(s1, 12, 10) + ", dude")
    }

    it("should do nothing when is used to check l1.size == 3") {
      org.scalactic.Requirements.require(l1.size == 3, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.size == 10, ", dude")
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(l1, 3, 10) + ", dude")
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      org.scalactic.Requirements.require(!(s1.size == 10), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(s1.size == 12), ", dude")
      }
      assert(e.getMessage == hadSize(s1, 12) + ", dude")
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      org.scalactic.Requirements.require(!(l1.size == 2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!(l1.size == 3), ", dude")
      }
      assert(e.getMessage == hadSize(l1, 3) + ", dude")
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      org.scalactic.Requirements.require(floatLengthSize.size == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(floatLengthSize.size == 1.0f, ", dude")
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f) + ", dude")
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      org.scalactic.Requirements.require(l1.exists(_ == 3), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.exists(_ == 5), ", dude")
      }
      assert(e.getMessage == didNotContain(l1, 5) + ", dude")
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      org.scalactic.Requirements.require(!l1.exists(_ == 5), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(!l1.exists(_ == 3), ", dude")
      }
      assert(e.getMessage == contained(l1, 3) + ", dude")
    }

    // SKIP-DOTTY-START
    // different printing of anonymous functions
    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l1.exists(_ > 3), ", dude")
      }
      assert(e.getMessage == wasFalse("l1.exists(((x$12: Int) => x$12.>(3)))") + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(l3.exists(_.isEmpty), ", dude")
      }
      assert(e.getMessage == wasFalse("l3.exists(((x$13: String) => x$13.isEmpty()))") + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(ci1.exists(321), ", dude")
      }
      assert(e.getMessage == wasFalse("ci1.exists(321)") + ", dude")
    }
    // SKIP-DOTTY-END

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      org.scalactic.Requirements.require(woof { meow(y = 5) } == "woof", ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[IllegalArgumentException] {
        org.scalactic.Requirements.require(woof { meow(y = 5) } == "meow", ", dude")
      }
      assert(e.getMessage == didNotEqual("woof", "meow") + ", dude")
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalactic.Requirements.require(org == "test", ", dude")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalactic.Requirements.require(org === "test", ", dude")
        """.stripMargin)
    }

    // SKIP-DOTTY-START
    // ambiguous implicit:
    //   both method convertToCheckingEqualizer in trait TypeCheckedTripleEquals and method convertToEqualizer in trait TripleEquals provide an extension method `===` on String(org)
    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends FunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    _root_.org.scalactic.Requirements.require(org === "test", ", dude")
          |  }
          |}
        """.stripMargin)
    }
    // SKIP-DOTTY-END

    it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
      assertCompiles(
        """
          |class Test {
          |  def aCustomMethod: Boolean = true
          |}
          |val org = new Test
          |_root_.org.scalactic.Requirements.require(org.aCustomMethod, ", dude")
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |_root_.org.scalactic.Requirements.require(!org, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.require(org.isEmpty, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.require(org.isInstanceOf[String], ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |_root_.org.scalactic.Requirements.require(org.size == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.require(org.length == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |_root_.org.scalactic.Requirements.require(org.exists(_ == 'b'), ", dude")
        """.stripMargin)
    }

  }

  describe("The requireState(boolean) method") {

    val a = 3
    val b = 5

    it("should do nothing when is used to check a == 3") {
      org.scalactic.Requirements.requireState(a == 3)
    }

    it("should throw IllegalStateException when is used to check a == 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 5 == b") {
      org.scalactic.Requirements.requireState(5 == b)
    }

    it("should throw IllegalStateException when is used to check 3 == b") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(3 == b)
      }
      assert(e.getMessage == didNotEqual(3, b))
    }

    it("should do nothing when is used to check 3 == 3") {
      org.scalactic.Requirements.requireState(3 == 3)
    }

    it("should throw IllegalStateException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(3 == 5)
      }
      assert(e.getMessage == "Expression was false")
    }

    it("should throw IllegalStateException when is used to check a == b") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == b)
      }
      assert(e.getMessage == didNotEqual(a, b))
    }

    it("should throw IllegalStateException when is used to check a == -1") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == -1)
      }
      assert(e.getMessage == didNotEqual(a, -1))
    }

    it("should throw IllegalStateException when is used to check -1 == a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(-1 == a)
      }
      assert(e.getMessage == didNotEqual(-1, a))
    }

    it("should do nothing when is used to check a === 3") {
      org.scalactic.Requirements.requireState(a === 3)
    }

    it("should throw IllegalStateException when is used to check a === 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a === 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 3 === a") {
      org.scalactic.Requirements.requireState(3 === a)
    }

    it("should throw IllegalStateException when is used to check 5 === a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(5 === a)
      }
      assert(e.getMessage == didNotEqual(5, a))
    }

    it("should do nothing when is used to check a !== 5") {
      org.scalactic.Requirements.requireState(a !== 5)
    }

    it("should throw IllegalStateException when is used to check a !== 3") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a !== 3)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should do nothing when is used to check 5 !== a") {
      org.scalactic.Requirements.requireState(5 !== a)
    }

    it("should throw IllegalStateException when is used to check 3 !== a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(3 !== a)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should do nothing when is used to check a > 2") {
      org.scalactic.Requirements.requireState(a > 2)
    }

    it("should do nothing when is used to check 5 > a") {
      org.scalactic.Requirements.requireState(5 > a)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a > 3)
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(3 > a)
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3))
    }

    it("should do nothing when is used to check a >= 3") {
      org.scalactic.Requirements.requireState(a >= 3)
    }

    it("should do nothing when is used to check 3 >= a") {
      org.scalactic.Requirements.requireState(3 >= a)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a >= 4)
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(3, 4))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(2 >= a)
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(2, 3))
    }

    it("should do nothing when is used to check b < 6") {
      org.scalactic.Requirements.requireState(b < 6)
    }

    it("should do nothing when is used to check 3 < b") {
      org.scalactic.Requirements.requireState(3 < b)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(b < 5)
      }
      assert(e.getMessage == wasNotLessThan(5, 5))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(5 < b)
      }
      assert(e.getMessage == wasNotLessThan(5, 5))
    }

    it("should do nothing when is used to check b <= 5") {
      org.scalactic.Requirements.requireState(b <= 5)
    }

    it("should do nothing when is used to check 5 <= b") {
      org.scalactic.Requirements.requireState(5 <= b)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(b <= 4)
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(5, 4))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(6 <= b)
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(6, 5))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      org.scalactic.Requirements.requireState(a == 3 && b == 5)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 3 && b == 6)
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 && b == 5)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 && b == 6)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      org.scalactic.Requirements.requireState(a == 3 & b == 5)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 3 & b == 6)
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 & b == 5)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 & b == 6)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      org.scalactic.Requirements.requireState(a == 3 || b == 5)
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      org.scalactic.Requirements.requireState(a == 3 || b == 6)
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      org.scalactic.Requirements.requireState(a == 2 || b == 5)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 || b == 6)
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      org.scalactic.Requirements.requireState(a == 3 | b == 5)
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      org.scalactic.Requirements.requireState(a == 3 | b == 6)
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      org.scalactic.Requirements.requireState(a == 2 | b == 5)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 | b == 6)
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      org.scalactic.Requirements.requireState(a == 3 && (b == 5 && b > 3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 3 && (b == 5 && b > 5))
      }
      assert(e.getMessage == commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))))
    }

    it("should do nothing when is used to check !(a == 5)") {
      org.scalactic.Requirements.requireState(!(a == 5))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(a == 3))
      }
      assert(e.getMessage == equaled(3, 3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 3 && !(b == 5))
      }
      assert(e.getMessage == commaBut(equaled(3, 3), equaled(5, 5)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      org.scalactic.Requirements.requireState((a == 3) == (b == 5))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState((a == 3) == (b != 5))
      }
      assert(e.getMessage === didNotEqual(true, false))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 5 && s.changeState)
      }
      assert(s.state == false)
    }

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 5 & s.changeState)
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      org.scalactic.Requirements.requireState(a == 3 || s.changeState)
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      org.scalactic.Requirements.requireState(a == 3 | s.changeState)
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      org.scalactic.Requirements.requireState(a == 3 && { println("hi"); b == 5})
    }

    it("should throw IllegalStateException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 3 && { println("hi"); b == 3})
      }
      if (ScalacticVersions.BuiltForScalaVersion == "2.12" || ScalacticVersions.BuiltForScalaVersion == "2.13")
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.Predef.println(\"hi\");" + lineSeparator + "  b.==(3)" + lineSeparator + "}")))
      else if (ScalacticVersions.BuiltForScalaVersion.startsWith("0."))
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.Predef.println(\"hi\")" + lineSeparator + "  b.==(3)" + lineSeparator + "}")))
      else
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.this.Predef.println(\"hi\");" + lineSeparator + "  b.==(3)" + lineSeparator + "}")))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      org.scalactic.Requirements.requireState({ println("hi"); b == 5} && a == 3)
    }

    it("should throw IllegalStateException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState({ println("hi"); b == 5} && a == 5)
      }
      if (ScalacticVersions.BuiltForScalaVersion == "2.12" || ScalacticVersions.BuiltForScalaVersion == "2.13")
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.Predef.println(\"hi\");" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)))
      else if (ScalacticVersions.BuiltForScalaVersion.startsWith("0."))
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.Predef.println(\"hi\")" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)))
      else
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.this.Predef.println(\"hi\");" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      org.scalactic.Requirements.requireState(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      org.scalactic.Requirements.requireState(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      org.scalactic.Requirements.requireState(neverRuns3(sys.error("Sad times 3"))(0))
    }

    val s1 = "hi ScalaTest"
    val s2 = "ScalaTest hi"
    val s3 = "Say hi to ScalaTest"
    val s4 = ""

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)
    val l2 = List.empty[Int]
    val l3 = List("one", "two", "three")

    val m1 = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val m2 = Map.empty[Int, String]

    val ct1 = new CustomContainer(8)

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      org.scalactic.Requirements.requireState(s1 startsWith "hi")
      org.scalactic.Requirements.requireState(s1.startsWith("hi"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s2 startsWith "hi")
      }
      assert(e1.getMessage == didNotStartWith(s2, "hi"))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s2.startsWith("hi"))
      }
      assert(e2.getMessage == didNotStartWith(s2, "hi"))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      org.scalactic.Requirements.requireState(ci1 startsWith 1)
      org.scalactic.Requirements.requireState(ci1.startsWith(1))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci2 startsWith 1)
      }
      assert(e1.getMessage == didNotStartWith(ci2, 1))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci2.startsWith(1))
      }
      assert(e2.getMessage == didNotStartWith(ci2, 1))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      org.scalactic.Requirements.requireState(!s2.startsWith("hi"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s1.startsWith("hi"))
      }
      assert(e1.getMessage == startedWith(s1, "hi"))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      org.scalactic.Requirements.requireState(s2 endsWith "hi")
      org.scalactic.Requirements.requireState(s2.endsWith("hi"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s1 endsWith "hi")
      }
      assert(e1.getMessage == didNotEndWith(s1, "hi"))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s1.endsWith("hi"))
      }
      assert(e2.getMessage == didNotEndWith(s1, "hi"))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      org.scalactic.Requirements.requireState(ci2 endsWith 1)
      org.scalactic.Requirements.requireState(ci2.endsWith(1))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1 endsWith 1)
      }
      assert(e1.getMessage == didNotEndWith(ci1, 1))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1.endsWith(1))
      }
      assert(e2.getMessage == didNotEndWith(ci1, 1))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      org.scalactic.Requirements.requireState(!s1.endsWith("hi"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s2.endsWith("hi"))
      }
      assert(e1.getMessage == endedWith(s2, "hi"))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      org.scalactic.Requirements.requireState(s3 contains "hi")
      org.scalactic.Requirements.requireState(s3.contains("hi"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s3 contains "hello")
      }
      assert(e1.getMessage == didNotContain(s3, "hello"))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s3.contains("hello"))
      }
      assert(e2.getMessage == didNotContain(s3, "hello"))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      org.scalactic.Requirements.requireState(ci2 contains 2)
      org.scalactic.Requirements.requireState(ci2.contains(2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1 contains 5)
      }
      assert(e1.getMessage == didNotContain(ci1, 5))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1.contains(5))
      }
      assert(e2.getMessage == didNotContain(ci1, 5))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      org.scalactic.Requirements.requireState(!s3.contains("hello"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s3.contains("hi"))
      }
      assert(e1.getMessage == contained(s3, "hi"))
    }

    it("should do nothing when is used to check l1 contains 2") {
      org.scalactic.Requirements.requireState(l1 contains 2)
      org.scalactic.Requirements.requireState(l1.contains(2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1 contains 5)
      }
      assert(e1.getMessage == didNotContain(l1, 5))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.contains(5))
      }
      assert(e2.getMessage == didNotContain(l1, 5))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      org.scalactic.Requirements.requireState(!(l1 contains 5))
      org.scalactic.Requirements.requireState(!l1.contains(5))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(l1 contains 2))
      }
      assert(e1.getMessage == contained(l1, 2))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!l1.contains(2))
      }
      assert(e2.getMessage == contained(l1, 2))
    }

    it("should do nothing when is used to check m1 contains 2") {
      org.scalactic.Requirements.requireState(m1 contains 2)
      org.scalactic.Requirements.requireState(m1.contains(2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(m1 contains 5)
      }
      assert(e1.getMessage == didNotContainKey(m1, 5))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(m1.contains(5))
      }
      assert(e2.getMessage == didNotContainKey(m1, 5))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      org.scalactic.Requirements.requireState(!(m1 contains 5))
      org.scalactic.Requirements.requireState(!m1.contains(5))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(m1 contains 2))
      }
      assert(e1.getMessage == containedKey(m1, 2))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!m1.contains(2))
      }
      assert(e2.getMessage == containedKey(m1, 2))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      org.scalactic.Requirements.requireState(ct1 contains 8)
      org.scalactic.Requirements.requireState(ct1.contains(8))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ct1 contains 5)
      }
      assert(e1.getMessage == didNotContain(ct1, 5))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ct1.contains(5))
      }
      assert(e2.getMessage == didNotContain(ct1, 5))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      org.scalactic.Requirements.requireState(!ct1.contains(5))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!ct1.contains(8))
      }
      assert(e1.getMessage == contained(ct1, 8))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      org.scalactic.Requirements.requireState(ci1 eq ci3)
      org.scalactic.Requirements.requireState(ci1.eq(ci3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1 eq ci2)
      }
      assert(e1.getMessage == wasNotTheSameInstanceAs(ci1, ci2))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1.eq(ci2))
      }
      assert(e2.getMessage == wasNotTheSameInstanceAs(ci1, ci2))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      org.scalactic.Requirements.requireState(!ci1.eq(ci2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!ci1.eq(ci3))
      }
      assert(e.getMessage == wasTheSameInstanceAs(ci1, ci3))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      org.scalactic.Requirements.requireState(ci1 ne ci2)
      org.scalactic.Requirements.requireState(ci1.ne(ci2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1 ne ci3)
      }
      assert(e1.getMessage == wasTheSameInstanceAs(ci1, ci3))

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1.ne(ci3))
      }
      assert(e2.getMessage == wasTheSameInstanceAs(ci1, ci3))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      org.scalactic.Requirements.requireState(!ci1.ne(ci3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!ci1.ne(ci2))
      }
      assert(e.getMessage == wasNotTheSameInstanceAs(ci1, ci2))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      org.scalactic.Requirements.requireState(s4.isEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s3.isEmpty)
      }
      assert(e.getMessage == wasNotEmpty(s3))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      org.scalactic.Requirements.requireState(!s3.isEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s4.isEmpty)
      }
      assert(e.getMessage == wasEmpty(s4))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      org.scalactic.Requirements.requireState(l2.isEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.isEmpty)
      }
      assert(e.getMessage == wasNotEmpty(l1))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      org.scalactic.Requirements.requireState(!l1.isEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!l2.isEmpty)
      }
      assert(e.getMessage == wasEmpty(l2))
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      org.scalactic.Requirements.requireState(s3.nonEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s4.nonEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s4.nonEmpty)
      }
      assert(e.getMessage == wasEmpty(s4))
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      org.scalactic.Requirements.requireState(!s4.nonEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s3.nonEmpty)
      }
      assert(e.getMessage == wasNotEmpty(s3))
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      org.scalactic.Requirements.requireState(l1.nonEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l2.nonEmpty)
      }
      assert(e.getMessage == wasEmpty(l2))
    }

    it("should do nothing when is used to check !l2.nonEmpty") {
      org.scalactic.Requirements.requireState(!l2.nonEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!l1.nonEmpty)
      }
      assert(e.getMessage == wasNotEmpty(l1))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      org.scalactic.Requirements.requireState(s1.isInstanceOf[String])
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.isInstanceOf[String])
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "scala.Predef.String"))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      org.scalactic.Requirements.requireState(l1.isInstanceOf[List[Int]])
    }

    // SKIP-DOTTY-START
    // different printing of `List[Int]`
    it("should throw IllegalStateException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s1.isInstanceOf[List[Int]])
      }
      assert(e.getMessage == wasNotInstanceOf(s1, "scala.List"))
    }
    // SKIP-DOTTY-END

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      org.scalactic.Requirements.requireState(date.isInstanceOf[Date])
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.isInstanceOf[Date])
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "java.util.Date"))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      org.scalactic.Requirements.requireState(!l1.isInstanceOf[String])
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s1.isInstanceOf[String])
      }
      assert(e.getMessage == wasInstanceOf(s1, "scala.Predef.String"))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      org.scalactic.Requirements.requireState(!s1.isInstanceOf[List[Int]])
    }

    // SKIP-DOTTY-START
    // different printing of `List[Int]`
    it("should throw IllegalStateException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!l1.isInstanceOf[List[Int]])
      }
      assert(e.getMessage == wasInstanceOf(l1, "scala.List"))
    }
    // SKIP-DOTTY-END

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      org.scalactic.Requirements.requireState(!l1.isInstanceOf[Date])
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!date.isInstanceOf[Date])
      }
      assert(e.getMessage == wasInstanceOf(date, "java.util.Date"))
    }

    it("should do nothing when is used to check s1.length == 9") {
      org.scalactic.Requirements.requireState(s1.length == 12)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s1.length == 10)
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(s1, 12, 10))
    }

    it("should do nothing when is used to check l1.length == 3") {
      org.scalactic.Requirements.requireState(l1.length == 3)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.length == 10)
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(l1, 3, 10))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      org.scalactic.Requirements.requireState(!(s1.length == 10))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(s1.length == 12))
      }
      assert(e.getMessage == hadLength(s1, 12))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      org.scalactic.Requirements.requireState(!(l1.length == 2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(l1.length == 3))
      }
      assert(e.getMessage == hadLength(l1, 3))
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      org.scalactic.Requirements.requireState(floatLengthSize.length == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(floatLengthSize.length == 1.0f)
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f))
    }

    it("should do nothing when is used to check s1.size == 9") {
      org.scalactic.Requirements.requireState(s1.size == 12)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s1.size == 10)
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(s1, 12, 10))
    }

    it("should do nothing when is used to check l1.size == 3") {
      org.scalactic.Requirements.requireState(l1.size == 3)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.size == 10)
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(l1, 3, 10))
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      org.scalactic.Requirements.requireState(!(s1.size == 10))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(s1.size == 12))
      }
      assert(e.getMessage == hadSize(s1, 12))
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      org.scalactic.Requirements.requireState(!(l1.size == 2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(l1.size == 3))
      }
      assert(e.getMessage == hadSize(l1, 3))
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      org.scalactic.Requirements.requireState(floatLengthSize.size == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(floatLengthSize.size == 1.0f)
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f))
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      org.scalactic.Requirements.requireState(l1.exists(_ == 3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.exists(_ == 5))
      }
      assert(e.getMessage == didNotContain(l1, 5))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      org.scalactic.Requirements.requireState(!l1.exists(_ == 5))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!l1.exists(_ == 3))
      }
      assert(e.getMessage == contained(l1, 3))
    }

    // SKIP-DOTTY-START
    // different printing of anonymous functions
    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.exists(_ > 3))
      }
      assert(e.getMessage == wasFalse("l1.exists(((x$18: Int) => x$18.>(3)))"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l3.exists(_.isEmpty))
      }
      assert(e.getMessage == wasFalse("l3.exists(((x$19: String) => x$19.isEmpty()))"))
    }
    // SKIP-DOTTY-END

    it("should throw IllegalStateException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1.exists(321))
      }
      assert(e.getMessage == wasFalse("ci1.exists(321)"))
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      org.scalactic.Requirements.requireState(woof { meow(y = 5) } == "woof")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(woof { meow(y = 5) } == "meow")
      }
      assert(e.getMessage == didNotEqual("woof", "meow"))
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalactic.Requirements.requireState(org == "test")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalactic.Requirements.requireState(org === "test")
        """.stripMargin)
    }

    // SKIP-DOTTY-START
    // ambiguous implicit:
    //   both method convertToCheckingEqualizer in trait TypeCheckedTripleEquals and method convertToEqualizer in trait TripleEquals provide an extension method `===` on String(org)
    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends FunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    _root_.org.scalactic.Requirements.requireState(org === "test")
          |  }
          |}
        """.stripMargin)
    }
    // SKIP-DOTTY-END

    it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
      assertCompiles(
        """
          |class Test {
          |  def aCustomMethod: Boolean = true
          |}
          |val org = new Test
          |_root_.org.scalactic.Requirements.requireState(org.aCustomMethod)
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |_root_.org.scalactic.Requirements.requireState(!org)
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.requireState(org.isEmpty)
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.requireState(org.isInstanceOf[String])
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |_root_.org.scalactic.Requirements.requireState(org.size == 0)
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.requireState(org.length == 0)
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |_root_.org.scalactic.Requirements.requireState(org.exists(_ == 'b'))
        """.stripMargin)
    }

  }

  describe("The requireState(boolean, clue) method") {

    val a = 3
    val b = 5

    it("should throw NullPointerException when null is passed in as clue") {
      val e = intercept[NullPointerException] {
        org.scalactic.Requirements.requireState(a == 3, null)
      }
      assert(e.getMessage == "clue was null")
    }

    it("should do nothing when is used to check a == 3") {
      org.scalactic.Requirements.requireState(a == 3, "dude")
    }

    it("should throw IllegalStateException when is used to check a == 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 5 == b") {
      org.scalactic.Requirements.requireState(5 == b, ", dude")
    }

    it("should throw IllegalStateException when is used to check 3 == b") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(3 == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, b) + ", dude")
    }

    it("should do nothing when is used to check a != 5") {
      org.scalactic.Requirements.requireState(a != 5, ". dude")
    }

    it("should throw IllegalStateException when is used to check a != 3") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a != 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 3 != b") {
      org.scalactic.Requirements.requireState(3 != b, "; dude")
    }

    it("should throw IllegalStateException when is used to check 5 != b") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(5 != b, "; dude")
      }
      assert(e.getMessage == equaled(5, b) + "; dude")
    }

    it("should do nothing when is used to check 3 == 3") {
      org.scalactic.Requirements.requireState(3 == 3, "dude")
    }

    it("should throw IllegalStateException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(3 == 5, "dude")
      }
      assert(e.getMessage == "dude")
    }

    it("should throw IllegalStateException when is used to check a == b") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(a, b) + ", dude")
    }

    it("should throw IllegalStateException when is used to check a == -1") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == -1, ". dude")
      }
      assert(e.getMessage == didNotEqual(a, -1) + ". dude")
    }

    it("should throw IllegalStateException when is used to check -1 == a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(-1 == a, "; dude")
      }
      assert(e.getMessage == didNotEqual(-1, a) + "; dude")
    }

    it("should throw IllegalStateException when is used to check 3 != a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(3 != a, ", dude")
      }
      assert(e.getMessage == equaled(3, a) + ", dude")
    }

    it("should do nothing when is used to check 5 != a") {
      org.scalactic.Requirements.requireState(5 != a, ". dude")
    }

    it("should do nothing when is used to check a === 3") {
      org.scalactic.Requirements.requireState(a === 3, "dude")
    }

    it("should throw IllegalStateException when is used to check a === 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a === 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 3 === a") {
      org.scalactic.Requirements.requireState(3 === a, ", dude")
    }

    it("should throw IllegalStateException when is used to check 5 === a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(5 === a, ", dude")
      }
      assert(e.getMessage == didNotEqual(5, a) + ", dude")
    }

    it("should do nothing when is used to check a !== 5") {
      org.scalactic.Requirements.requireState(a !== 5, ". dude")
    }

    it("should throw IllegalStateException when is used to check a !== 3") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a !== 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 5 !== a") {
      org.scalactic.Requirements.requireState(5 !== a, "; dude")
    }

    it("should throw IllegalStateException when is used to check 3 !== a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(3 !== a, "; dude")
      }
      assert(e.getMessage == equaled(3, a) + "; dude")
    }

    it("should do nothing when is used to check a > 2") {
      org.scalactic.Requirements.requireState(a > 2, ", dude")
    }

    it("should do nothing when is used to check 5 > a") {
      org.scalactic.Requirements.requireState(5 > a, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a > 3, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(3 > a, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3) + ", dude")
    }

    it("should do nothing when is used to check a >= 3") {
      org.scalactic.Requirements.requireState(a >= 3, ", dude")
    }

    it("should do nothing when is used to check 3 >= a") {
      org.scalactic.Requirements.requireState(3 >= a, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a >= 4, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(3, 4) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(2 >= a, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(2, 3) + ", dude")
    }

    it("should do nothing when is used to check b < 6") {
      org.scalactic.Requirements.requireState(b < 6, ", dude")
    }

    it("should do nothing when is used to check 3 < b") {
      org.scalactic.Requirements.requireState(3 < b, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(b < 5, ", dude")
      }
      assert(e.getMessage == wasNotLessThan(5, 5) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(5 < b, ", dude")
      }
      assert(e.getMessage == wasNotLessThan(5, 5) + ", dude")
    }

    it("should do nothing when is used to check b <= 5") {
      org.scalactic.Requirements.requireState(b <= 5, ", dude")
    }

    it("should do nothing when is used to check 5 <= b") {
      org.scalactic.Requirements.requireState(5 <= b, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(b <= 4, ", dude")
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(5, 4) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(6 <= b, ", dude")
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(6, 5) + ", dude")
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      org.scalactic.Requirements.requireState(a == 3 && b == 5, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 3 && b == 6, ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 && b == 5, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 && b == 6, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      org.scalactic.Requirements.requireState(a == 3 & b == 5, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 3 & b == 6, ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 & b == 5, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 & b == 6, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      org.scalactic.Requirements.requireState(a == 3 || b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      org.scalactic.Requirements.requireState(a == 3 || b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      org.scalactic.Requirements.requireState(a == 2 || b == 5, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 || b == 6, ", dude")
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      org.scalactic.Requirements.requireState(a == 3 | b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      org.scalactic.Requirements.requireState(a == 3 | b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      org.scalactic.Requirements.requireState(a == 2 | b == 5, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 2 | b == 6, ", dude")
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude")
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      org.scalactic.Requirements.requireState(a == 3 && (b == 5 && b > 3), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 3 && (b == 5 && b > 5), ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))) + ", dude")
    }

    it("should do nothing when is used to check !(a == 5)") {
      org.scalactic.Requirements.requireState(!(a == 5), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(a == 3), ", dude")
      }
      assert(e.getMessage == equaled(3, 3) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 3 && !(b == 5), ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), equaled(5, 5)) + ", dude")
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      org.scalactic.Requirements.requireState((a == 3) == (b == 5), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState((a == 3) == (b != 5), ", dude")
      }
      assert(e.getMessage === didNotEqual(true, false) + ", dude")
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 5 && s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 5 & s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      org.scalactic.Requirements.requireState(a == 3 || s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      org.scalactic.Requirements.requireState(a == 3 | s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      org.scalactic.Requirements.requireState(a == 3 && { println("hi"); b == 5}, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(a == 3 && { println("hi"); b == 3}, ", dude")
      }
      if (ScalacticVersions.BuiltForScalaVersion == "2.12" || ScalacticVersions.BuiltForScalaVersion == "2.13")
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.Predef.println(\"hi\");" + lineSeparator + "  b.==(3)" + lineSeparator + "}")) + ", dude")
      else if (ScalacticVersions.BuiltForScalaVersion.startsWith("0."))
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.Predef.println(\"hi\")" + lineSeparator + "  b.==(3)" + lineSeparator + "}")) + ", dude")
      else
        assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{" + lineSeparator + "  scala.this.Predef.println(\"hi\");" + lineSeparator + "  b.==(3)" + lineSeparator + "}")) + ", dude")
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      org.scalactic.Requirements.requireState({ println("hi"); b == 5} && a == 3, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState({ println("hi"); b == 5} && a == 5, ", dude")
      }
      if (ScalacticVersions.BuiltForScalaVersion == "2.12" || ScalacticVersions.BuiltForScalaVersion == "2.13")
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.Predef.println(\"hi\");" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)) + ", dude")
      else if (ScalacticVersions.BuiltForScalaVersion.startsWith("0."))
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.Predef.println(\"hi\")" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)) + ", dude")
      else
        assert(e.getMessage == commaBut(wasTrue("{" + lineSeparator + "  scala.this.Predef.println(\"hi\");" + lineSeparator + "  b.==(5)" + lineSeparator + "}"), didNotEqual(3, 5)) + ", dude")
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      org.scalactic.Requirements.requireState(neverRuns1(sys.error("Sad times 1")), "should not fail!")
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      org.scalactic.Requirements.requireState(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      org.scalactic.Requirements.requireState(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
    }

    val s1 = "hi ScalaTest"
    val s2 = "ScalaTest hi"
    val s3 = "Say hi to ScalaTest"
    val s4 = ""

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)
    val l2 = List.empty[Int]
    val l3 = List("one", "two", "three")

    val m1 = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val m2 = Map.empty[Int, String]

    val ct1 = new CustomContainer(8)

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      org.scalactic.Requirements.requireState(s1 startsWith "hi", ", dude")
      org.scalactic.Requirements.requireState(s1.startsWith("hi"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s2 startsWith "hi", ", dude")
      }
      assert(e1.getMessage == didNotStartWith(s2, "hi") + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s2.startsWith("hi"), ", dude")
      }
      assert(e2.getMessage == didNotStartWith(s2, "hi") + ", dude")
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      org.scalactic.Requirements.requireState(ci1 startsWith 1, ", dude")
      org.scalactic.Requirements.requireState(ci1.startsWith(1), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci2 startsWith 1, ", dude")
      }
      assert(e1.getMessage == didNotStartWith(ci2, 1) + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci2.startsWith(1), ", dude")
      }
      assert(e2.getMessage == didNotStartWith(ci2, 1) + ", dude")
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      org.scalactic.Requirements.requireState(!s2.startsWith("hi"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s1.startsWith("hi"), ", dude")
      }
      assert(e1.getMessage == startedWith(s1, "hi") + ", dude")
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      org.scalactic.Requirements.requireState(s2 endsWith "hi", ", dude")
      org.scalactic.Requirements.requireState(s2.endsWith("hi"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s1 endsWith "hi", ", dude")
      }
      assert(e1.getMessage == didNotEndWith(s1, "hi") + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s1.endsWith("hi"), ", dude")
      }
      assert(e2.getMessage == didNotEndWith(s1, "hi") + ", dude")
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      org.scalactic.Requirements.requireState(ci2 endsWith 1, ", dude")
      org.scalactic.Requirements.requireState(ci2.endsWith(1), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1 endsWith 1, ", dude")
      }
      assert(e1.getMessage == didNotEndWith(ci1, 1) + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1.endsWith(1), ", dude")
      }
      assert(e2.getMessage == didNotEndWith(ci1, 1) + ", dude")
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      org.scalactic.Requirements.requireState(!s1.endsWith("hi"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s2.endsWith("hi"), ", dude")
      }
      assert(e1.getMessage == endedWith(s2, "hi") + ", dude")
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      org.scalactic.Requirements.requireState(s3 contains "hi", ", dude")
      org.scalactic.Requirements.requireState(s3.contains("hi"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s3 contains "hello", ", dude")
      }
      assert(e1.getMessage == didNotContain(s3, "hello") + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s3.contains("hello"), ", dude")
      }
      assert(e2.getMessage == didNotContain(s3, "hello") + ", dude")
    }

    it("should do nothing when is used to check ci2 contains 2") {
      org.scalactic.Requirements.requireState(ci2 contains 2, ", dude")
      org.scalactic.Requirements.requireState(ci2.contains(2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContain(ci1, 5) + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContain(ci1, 5) + ", dude")
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      org.scalactic.Requirements.requireState(!s3.contains("hello"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s3.contains("hi"), ", dude")
      }
      assert(e1.getMessage == contained(s3, "hi") + ", dude")
    }

    it("should do nothing when is used to check l1 contains 2") {
      org.scalactic.Requirements.requireState(l1 contains 2, ", dude")
      org.scalactic.Requirements.requireState(l1.contains(2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContain(l1, 5) + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContain(l1, 5) + ", dude")
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      org.scalactic.Requirements.requireState(!(l1 contains 5), ", dude")
      org.scalactic.Requirements.requireState(!l1.contains(5), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(l1 contains 2), ", dude")
      }
      assert(e1.getMessage == contained(l1, 2) + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!l1.contains(2), ", dude")
      }
      assert(e2.getMessage == contained(l1, 2) + ", dude")
    }

    it("should do nothing when is used to check m1 contains 2") {
      org.scalactic.Requirements.requireState(m1 contains 2, ", dude")
      org.scalactic.Requirements.requireState(m1.contains(2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(m1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContainKey(m1, 5) + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(m1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContainKey(m1, 5) + ", dude")
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      org.scalactic.Requirements.requireState(!(m1 contains 5), ", dude")
      org.scalactic.Requirements.requireState(!m1.contains(5), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(m1 contains 2), ", dude")
      }
      assert(e1.getMessage == containedKey(m1, 2) + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!m1.contains(2), ", dude")
      }
      assert(e2.getMessage == containedKey(m1, 2) + ", dude")
    }

    it("should do nothing when is used to check ct1 contains 8") {
      org.scalactic.Requirements.requireState(ct1 contains 8, ", dude")
      org.scalactic.Requirements.requireState(ct1.contains(8), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ct1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContain(ct1, 5) + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ct1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContain(ct1, 5) + ", dude")
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      org.scalactic.Requirements.requireState(!ct1.contains(5), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!ct1.contains(8), ", dude")
      }
      assert(e1.getMessage == contained(ct1, 8) + ", dude")
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      org.scalactic.Requirements.requireState(ci1 eq ci3, ", dude")
      org.scalactic.Requirements.requireState(ci1.eq(ci3), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1 eq ci2, ", dude")
      }
      assert(e1.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1.eq(ci2), ", dude")
      }
      assert(e2.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      org.scalactic.Requirements.requireState(!ci1.eq(ci2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!ci1.eq(ci3), ", dude")
      }
      assert(e.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      org.scalactic.Requirements.requireState(ci1 ne ci2, ", dude")
      org.scalactic.Requirements.requireState(ci1.ne(ci2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1 ne ci3, ", dude")
      }
      assert(e1.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")

      val e2 = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1.ne(ci3), ", dude")
      }
      assert(e2.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      org.scalactic.Requirements.requireState(!ci1.ne(ci3), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!ci1.ne(ci2), ", dude")
      }
      assert(e.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")
    }

    it("should do nothing when is used to check s4.isEmpty") {
      org.scalactic.Requirements.requireState(s4.isEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s3.isEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(s3) + ", dude")
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      org.scalactic.Requirements.requireState(!s3.isEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s4.isEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(s4) + ", dude")
    }

    it("should do nothing when is used to check l2.isEmpty") {
      org.scalactic.Requirements.requireState(l2.isEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.isEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(l1) + ", dude")
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      org.scalactic.Requirements.requireState(!l1.isEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!l2.isEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(l2) + ", dude")
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      org.scalactic.Requirements.requireState(s3.nonEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s4.nonEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s4.nonEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(s4) + ", dude")
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      org.scalactic.Requirements.requireState(!s4.nonEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s3.nonEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(s3) + ", dude")
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      org.scalactic.Requirements.requireState(l1.nonEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l2.nonEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(l2) + ", dude")
    }

    it("should do nothing when is used to check !l2.nonEmpty") {
      org.scalactic.Requirements.requireState(!l2.nonEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!l1.nonEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(l1) + ", dude")
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      org.scalactic.Requirements.requireState(s1.isInstanceOf[String], ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.isInstanceOf[String], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "scala.Predef.String") + ", dude")
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      org.scalactic.Requirements.requireState(l1.isInstanceOf[List[Int]], ", dude")
    }

    // SKIP-DOTTY-START
    // different printing of `List[Int]`
    it("should throw IllegalStateException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(s1, "scala.List") + ", dude")
    }
    // SKIP-DOTTY-END

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      org.scalactic.Requirements.requireState(date.isInstanceOf[Date], ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.isInstanceOf[Date], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "java.util.Date") + ", dude")
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      org.scalactic.Requirements.requireState(!l1.isInstanceOf[String], ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!s1.isInstanceOf[String], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(s1, "scala.Predef.String") + ", dude")
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      org.scalactic.Requirements.requireState(!s1.isInstanceOf[List[Int]], ", dude")
    }

    // SKIP-DOTTY-START
    // different printing of `List[Int]`
    it("should throw IllegalStateException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!l1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(l1, "scala.List") + ", dude")
    }
    // SKIP-DOTTY-END

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      org.scalactic.Requirements.requireState(!l1.isInstanceOf[Date], ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!date.isInstanceOf[Date], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(date, "java.util.Date") + ", dude")
    }

    it("should do nothing when is used to check s1.length == 9") {
      org.scalactic.Requirements.requireState(s1.length == 12, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s1.length == 10, ", dude")
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(s1, 12, 10) + ", dude")
    }

    it("should do nothing when is used to check l1.length == 3") {
      org.scalactic.Requirements.requireState(l1.length == 3, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.length == 10, ", dude")
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(l1, 3, 10) + ", dude")
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      org.scalactic.Requirements.requireState(!(s1.length == 10), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(s1.length == 12), ", dude")
      }
      assert(e.getMessage == hadLength(s1, 12) + ", dude")
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      org.scalactic.Requirements.requireState(!(l1.length == 2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(l1.length == 3), ", dude")
      }
      assert(e.getMessage == hadLength(l1, 3) + ", dude")
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      org.scalactic.Requirements.requireState(floatLengthSize.length == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(floatLengthSize.length == 1.0f, ", dude")
      }
      assert(e.getMessage == hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f) + ", dude")
    }

    it("should do nothing when is used to check s1.size == 9") {
      org.scalactic.Requirements.requireState(s1.size == 12, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(s1.size == 10, ", dude")
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(s1, 12, 10) + ", dude")
    }

    it("should do nothing when is used to check l1.size == 3") {
      org.scalactic.Requirements.requireState(l1.size == 3, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.size == 10, ", dude")
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(l1, 3, 10) + ", dude")
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      org.scalactic.Requirements.requireState(!(s1.size == 10), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(s1.size == 12), ", dude")
      }
      assert(e.getMessage == hadSize(s1, 12) + ", dude")
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      org.scalactic.Requirements.requireState(!(l1.size == 2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!(l1.size == 3), ", dude")
      }
      assert(e.getMessage == hadSize(l1, 3) + ", dude")
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      org.scalactic.Requirements.requireState(floatLengthSize.size == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(floatLengthSize.size == 1.0f, ", dude")
      }
      assert(e.getMessage == hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f) + ", dude")
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      org.scalactic.Requirements.requireState(l1.exists(_ == 3), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.exists(_ == 5), ", dude")
      }
      assert(e.getMessage == didNotContain(l1, 5) + ", dude")
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      org.scalactic.Requirements.requireState(!l1.exists(_ == 5), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(!l1.exists(_ == 3), ", dude")
      }
      assert(e.getMessage == contained(l1, 3) + ", dude")
    }

    // SKIP-DOTTY-START
    // different printing of anonymous functions
    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l1.exists(_ > 3), ", dude")
      }
      assert(e.getMessage == wasFalse("l1.exists(((x$24: Int) => x$24.>(3)))") + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(l3.exists(_.isEmpty), ", dude")
      }
      assert(e.getMessage == wasFalse("l3.exists(((x$25: String) => x$25.isEmpty()))") + ", dude")
    }
    // SKIP-DOTTY-END

    it("should throw IllegalStateException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(ci1.exists(321), ", dude")
      }
      assert(e.getMessage == wasFalse("ci1.exists(321)") + ", dude")
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      org.scalactic.Requirements.requireState(woof { meow(y = 5) } == "woof", ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[IllegalStateException] {
        org.scalactic.Requirements.requireState(woof { meow(y = 5) } == "meow", ", dude")
      }
      assert(e.getMessage == didNotEqual("woof", "meow") + ", dude")
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalactic.Requirements.requireState(org == "test", ", dude")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalactic.Requirements.requireState(org === "test", ", dude")
        """.stripMargin)
    }

    // SKIP-DOTTY-START
    // ambiguous implicit:
    //   both method convertToCheckingEqualizer in trait TypeCheckedTripleEquals and method convertToEqualizer in trait TripleEquals provide an extension method `===` on String(org)
    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends FunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    _root_.org.scalactic.Requirements.requireState(org === "test", ", dude")
          |  }
          |}
        """.stripMargin)
    }
    // SKIP-DOTTY-END

    it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
      assertCompiles(
        """
          |class Test {
          |  def aCustomMethod: Boolean = true
          |}
          |val org = new Test
          |_root_.org.scalactic.Requirements.requireState(org.aCustomMethod, ", dude")
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |_root_.org.scalactic.Requirements.requireState(!org, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.requireState(org.isEmpty, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.requireState(org.isInstanceOf[String], ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |_root_.org.scalactic.Requirements.requireState(org.size == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalactic.Requirements.requireState(org.length == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |_root_.org.scalactic.Requirements.requireState(org.exists(_ == 'b'), ", dude")
        """.stripMargin)
    }

  }

  describe("The requireNonNull(...) method") {

    val prefix = "prefix text"
    val string = "some text"
    val suffix = "suffix tex"

    val nullPrefix = null
    val nullString = null
    val nullSuffix = null

    it("should do nothing when passed in parameters are non-null") {
      org.scalactic.Requirements.requireNonNull(prefix, string, suffix)
    }

    it("should throw NullArgumentException with correct message when one of passed parameters is null") {
      val e = intercept[NullArgumentException] {
        org.scalactic.Requirements.requireNonNull(prefix, nullString, suffix)
      }
      assert(e.getMessage == "nullString was null")
    }

    it("should throw NullArgumentException with correct message when two of passed parameters is null") {
      val e = intercept[NullArgumentException] {
        org.scalactic.Requirements.requireNonNull(nullPrefix, nullString, suffix)
      }
      assert(e.getMessage == "nullPrefix and nullString were null")
    }

    it("should throw NullArgumentException with correct message when three of passed parameters is null") {
      val e = intercept[NullArgumentException] {
        org.scalactic.Requirements.requireNonNull(nullPrefix, nullString, nullSuffix)
      }
      assert(e.getMessage == "nullPrefix, nullString, and nullSuffix were null")
    }

    it("should throw NullArgumentException with correct message when one of passed parameters through object property is null") {
      class AClass {
        val aNull: String = null
      }
      val aClass = new AClass
      val e = intercept[NullArgumentException] {
        org.scalactic.Requirements.requireNonNull(prefix, aClass.aNull, suffix)
      }
      assert(e.getMessage == "aClass.aNull was null")
    }

    it("should throw NullArgumentException with correct message when one of passed parameters through object method call is null") {
      class AClass {
        def returnNull: String = null
      }
      val aClass = new AClass
      val e = intercept[NullArgumentException] {
        org.scalactic.Requirements.requireNonNull(prefix, aClass.returnNull, suffix)
      }
      assert(e.getMessage == "aClass.returnNull was null")
    }

    it("should throw NullArgumentException with correct message when one of passed parameters through method call is null") {
      def returnNull: String = null
      val e = intercept[NullArgumentException] {
        org.scalactic.Requirements.requireNonNull(prefix, returnNull, suffix)
      }
      assert(e.getMessage == "returnNull was null")
    }

    it("should compile correctly when variable with name 'org' is passed in") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalactic.Requirements.requireNonNull(org)
        """.stripMargin
      )
    }

  }

}