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
package org.scalautils

import org.scalatest._
import java.util.Date

class RequirementsSpec extends FunSpec with Requirements with OptionValues {

  private def neverRuns1(f: => Unit): Boolean = true
  private def neverRuns2(f: => Unit)(a: Int): Boolean = true
  private def neverRuns3[T](f: => Unit)(a: T): Boolean = true

  def didNotEqual(left: Any, right: Any): String = {
    val (leftee, rightee) = Prettifier.getObjectsForFailureMessage(left, right)
    FailureMessages("didNotEqual", leftee, rightee)
  }

  def equaled(left: Any, right: Any): String =
    FailureMessages("equaled", left, right)

  def expressionFailed(left: String): String =
    FailureMessages("expressionFailed", UnquotedString(left))

  def wasNotGreaterThan(left: Any, right: Any): String =
    FailureMessages("wasNotGreaterThan", left, right)

  def wasGreaterThan(left: Any, right: Any): String =
    FailureMessages("wasGreaterThan", left, right)

  def wasNotGreaterThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages("wasNotGreaterThanOrEqualTo", left, right)

  def wasGreaterThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages("wasGreaterThanOrEqualTo", left, right)

  def wasNotLessThan(left: Any, right: Any): String =
    FailureMessages("wasNotLessThan", left, right)

  def wasLessThan(left: Any, right: Any): String =
    FailureMessages("wasLessThan", left, right)

  def wasNotLessThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages("wasNotLessThanOrEqualTo", left, right)

  def wasLessThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages("wasLessThanOrEqualTo", left, right)

  def commaAnd(left: String, right: String): String =
    FailureMessages("commaAnd", UnquotedString(left), UnquotedString(right))

  def commaBut(left: String, right: String): String =
    FailureMessages("commaBut", UnquotedString(left), UnquotedString(right))

  def wasFalse(left: String): String =
    left + " was false"

  def wasTrue(left: String): String =
    left + " was true"

  def didNotStartWith(left: Any, right: Any): String =
    FailureMessages("didNotStartWith", left, right)

  def startedWith(left: Any, right: Any): String =
    FailureMessages("startedWith", left, right)

  def didNotEndWith(left: Any, right: Any): String =
    FailureMessages("didNotEndWith", left, right)

  def endedWith(left: Any, right: Any): String =
    FailureMessages("endedWith", left, right)

  def didNotContain(left: Any, right: Any): String =
    FailureMessages("didNotContain", left, right)

  def contained(left: Any, right: Any): String =
    FailureMessages("contained", left, right)

  def wasNotTheSameInstanceAs(left: AnyRef, right: AnyRef): String =
    FailureMessages("wasNotTheSameInstanceAs", left, right)

  def wasTheSameInstanceAs(left: AnyRef, right: AnyRef): String =
    FailureMessages("wasTheSameInstanceAs", left, right)

  def wasNotEmpty(left: Any): String =
    FailureMessages("wasNotEmpty", left)

  def wasEmpty(left: Any): String =
    FailureMessages("wasEmpty", left)

  def wasNotInstanceOf(left: Any, className: String): String =
    FailureMessages("wasNotInstanceOf", left, UnquotedString(className))

  def wasInstanceOf(left: Any, className: String): String =
    FailureMessages("wasInstanceOf", left, UnquotedString(className))

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

    override def toString: String = value.toString
  }

  describe("The require(boolean) method") {

    val a = 3
    val b = 5

    it("should do nothing when is used to check a == 3") {
      require(a == 3)
    }

    it("should throw IllegalArgumentException when is used to check a == 5") {
      val e = intercept[IllegalArgumentException] {
        require(a == 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 5 == b") {
      require(5 == b)
    }

    it("should throw IllegalArgumentException when is used to check 3 == b") {
      val e = intercept[IllegalArgumentException] {
        require(3 == b)
      }
      assert(e.getMessage == didNotEqual(3, b))
    }

    it("should do nothing when is used to check 3 == 3") {
      require(3 == 3)
    }

    it("should throw IllegalArgumentException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalArgumentException] {
        require(3 == 5)
      }
      assert(e.getMessage == "Expression was false")
    }

    it("should throw IllegalArgumentException when is used to check a == b") {
      val e = intercept[IllegalArgumentException] {
        require(a == b)
      }
      assert(e.getMessage == didNotEqual(a, b))
    }

    it("should throw IllegalArgumentException when is used to check a == null") {
      val e = intercept[IllegalArgumentException] {
        require(a == null)
      }
      assert(e.getMessage == didNotEqual(a, null))
    }

    it("should throw IllegalArgumentException when is used to check null == a") {
      val e = intercept[IllegalArgumentException] {
        require(null == a)
      }
      assert(e.getMessage == didNotEqual(null, a))
    }

    it("should do nothing when is used to check a === 3") {
      require(a === 3)
    }

    it("should throw IllegalArgumentException when is used to check a === 5") {
      val e = intercept[IllegalArgumentException] {
        require(a === 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 3 === a") {
      require(3 === a)
    }

    it("should throw IllegalArgumentException when is used to check 5 === a") {
      val e = intercept[IllegalArgumentException] {
        require(5 === a)
      }
      assert(e.getMessage == didNotEqual(5, a))
    }

    it("should do nothing when is used to check a !== 5") {
      require(a !== 5)
    }

    it("should throw IllegalArgumentException when is used to check a !== 3") {
      val e = intercept[IllegalArgumentException] {
        require(a !== 3)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should do nothing when is used to check 5 !== a") {
      require(5 !== a)
    }

    it("should throw IllegalArgumentException when is used to check 3 !== a") {
      val e = intercept[IllegalArgumentException] {
        require(3 !== a)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should do nothing when is used to check a > 2") {
      require(a > 2)
    }

    it("should do nothing when is used to check 5 > a") {
      require(5 > a)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[IllegalArgumentException] {
        require(a > 3)
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[IllegalArgumentException] {
        require(3 > a)
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3))
    }

    it("should do nothing when is used to check a >= 3") {
      require(a >= 3)
    }

    it("should do nothing when is used to check 3 >= a") {
      require(3 >= a)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[IllegalArgumentException] {
        require(a >= 4)
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(3, 4))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[IllegalArgumentException] {
        require(2 >= a)
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(2, 3))
    }

    it("should do nothing when is used to check b < 6") {
      require(b < 6)
    }

    it("should do nothing when is used to check 3 < b") {
      require(3 < b)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[IllegalArgumentException] {
        require(b < 5)
      }
      assert(e.getMessage == wasNotLessThan(5, 5))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[IllegalArgumentException] {
        require(5 < b)
      }
      assert(e.getMessage == wasNotLessThan(5, 5))
    }

    it("should do nothing when is used to check b <= 5") {
      require(b <= 5)
    }

    it("should do nothing when is used to check 5 <= b") {
      require(5 <= b)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[IllegalArgumentException] {
        require(b <= 4)
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(5, 4))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[IllegalArgumentException] {
        require(6 <= b)
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(6, 5))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      require(a == 3 && b == 5)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 3 && b == 6)
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 && b == 5)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 && b == 6)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      require(a == 3 & b == 5)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 3 & b == 6)
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 & b == 5)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 & b == 6)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      require(a == 3 || b == 5)
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      require(a == 3 || b == 6)
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      require(a == 2 || b == 5)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 || b == 6)
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      require(a == 3 | b == 5)
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      require(a == 3 | b == 6)
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      require(a == 2 | b == 5)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 | b == 6)
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      require(a == 3 && (b == 5 && b > 3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[IllegalArgumentException] {
        require(a == 3 && (b == 5 && b > 5))
      }
      assert(e.getMessage == commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))))
    }

    it("should do nothing when is used to check !(a == 5)") {
      require(!(a == 5))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[IllegalArgumentException] {
        require(!(a == 3))
      }
      assert(e.getMessage == equaled(3, 3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[IllegalArgumentException] {
        require(a == 3 && !(b == 5))
      }
      assert(e.getMessage == commaBut(equaled(3, 3), equaled(5, 5)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      require((a == 3) == (b == 5))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[IllegalArgumentException] {
        require((a == 3) == (b != 5))
      }
      assert(e.getMessage === didNotEqual(true, false))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[IllegalArgumentException] {
        require(a == 5 && s.changeState)
      }
      assert(s.state == false)
    }

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[IllegalArgumentException] {
        require(a == 5 & s.changeState)
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      require(a == 3 || s.changeState)
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      require(a == 3 | s.changeState)
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      require(a == 3 && { println("hi"); b == 5})
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[IllegalArgumentException] {
        require(a == 3 && { println("hi"); b == 3})
      }
      assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{\n  scala.this.Predef.println(\"hi\");\n  b.==(3)\n}")))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      require({ println("hi"); b == 5} && a == 3)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[IllegalArgumentException] {
        require({ println("hi"); b == 5} && a == 5)
      }
      assert(e.getMessage == commaBut(wasTrue("{\n  scala.this.Predef.println(\"hi\");\n  b.==(5)\n}"), didNotEqual(3, 5)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      require(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      require(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      require(neverRuns3(sys.error("Sad times 3"))(0))
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

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      require(s1 startsWith "hi")
      require(s1.startsWith("hi"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[IllegalArgumentException] {
        require(s2 startsWith "hi")
      }
      assert(e1.getMessage == didNotStartWith(s2, "hi"))

      val e2 = intercept[IllegalArgumentException] {
        require(s2.startsWith("hi"))
      }
      assert(e2.getMessage == didNotStartWith(s2, "hi"))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      require(ci1 startsWith 1)
      require(ci1.startsWith(1))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[IllegalArgumentException] {
        require(ci2 startsWith 1)
      }
      assert(e1.getMessage == didNotStartWith(ci2, 1))

      val e2 = intercept[IllegalArgumentException] {
        require(ci2.startsWith(1))
      }
      assert(e2.getMessage == didNotStartWith(ci2, 1))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      require(!s2.startsWith("hi"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        require(!s1.startsWith("hi"))
      }
      assert(e1.getMessage == startedWith(s1, "hi"))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      require(s2 endsWith "hi")
      require(s2.endsWith("hi"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[IllegalArgumentException] {
        require(s1 endsWith "hi")
      }
      assert(e1.getMessage == didNotEndWith(s1, "hi"))

      val e2 = intercept[IllegalArgumentException] {
        require(s1.endsWith("hi"))
      }
      assert(e2.getMessage == didNotEndWith(s1, "hi"))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      require(ci2 endsWith 1)
      require(ci2.endsWith(1))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[IllegalArgumentException] {
        require(ci1 endsWith 1)
      }
      assert(e1.getMessage == didNotEndWith(ci1, 1))

      val e2 = intercept[IllegalArgumentException] {
        require(ci1.endsWith(1))
      }
      assert(e2.getMessage == didNotEndWith(ci1, 1))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      require(!s1.endsWith("hi"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        require(!s2.endsWith("hi"))
      }
      assert(e1.getMessage == endedWith(s2, "hi"))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      require(s3 contains "hi")
      require(s3.contains("hi"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[IllegalArgumentException] {
        require(s3 contains "hello")
      }
      assert(e1.getMessage == didNotContain(s3, "hello"))

      val e2 = intercept[IllegalArgumentException] {
        require(s3.contains("hello"))
      }
      assert(e2.getMessage == didNotContain(s3, "hello"))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      require(ci2 contains 2)
      require(ci2.contains(2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        require(ci1 contains 5)
      }
      assert(e1.getMessage == didNotContain(ci1, 5))

      val e2 = intercept[IllegalArgumentException] {
        require(ci1.contains(5))
      }
      assert(e2.getMessage == didNotContain(ci1, 5))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      require(!s3.contains("hello"))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        require(!s3.contains("hi"))
      }
      assert(e1.getMessage == contained(s3, "hi"))
    }

    it("should do nothing when is used to check l1 contains 2") {
      require(l1 contains 2)
      require(l1.contains(2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        require(l1 contains 5)
      }
      assert(e1.getMessage == didNotContain(l1, 5))

      val e2 = intercept[IllegalArgumentException] {
        require(l1.contains(5))
      }
      assert(e2.getMessage == didNotContain(l1, 5))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      require(ci1 eq ci3)
      require(ci1.eq(ci3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[IllegalArgumentException] {
        require(ci1 eq ci2)
      }
      assert(e1.getMessage == wasNotTheSameInstanceAs(ci1, ci2))

      val e2 = intercept[IllegalArgumentException] {
        require(ci1.eq(ci2))
      }
      assert(e2.getMessage == wasNotTheSameInstanceAs(ci1, ci2))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      require(!ci1.eq(ci2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[IllegalArgumentException] {
        require(!ci1.eq(ci3))
      }
      assert(e.getMessage == wasTheSameInstanceAs(ci1, ci3))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      require(ci1 ne ci2)
      require(ci1.ne(ci2))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[IllegalArgumentException] {
        require(ci1 ne ci3)
      }
      assert(e1.getMessage == wasTheSameInstanceAs(ci1, ci3))

      val e2 = intercept[IllegalArgumentException] {
        require(ci1.ne(ci3))
      }
      assert(e2.getMessage == wasTheSameInstanceAs(ci1, ci3))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      require(!ci1.ne(ci3))
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[IllegalArgumentException] {
        require(!ci1.ne(ci2))
      }
      assert(e.getMessage == wasNotTheSameInstanceAs(ci1, ci2))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      require(s4.isEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        require(s3.isEmpty)
      }
      assert(e.getMessage == wasNotEmpty(s3))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      require(!s3.isEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        require(!s4.isEmpty)
      }
      assert(e.getMessage == wasEmpty(s4))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      require(l2.isEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        require(l1.isEmpty)
      }
      assert(e.getMessage == wasNotEmpty(l1))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      require(!l1.isEmpty)
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        require(!l2.isEmpty)
      }
      assert(e.getMessage == wasEmpty(l2))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      require(s1.isInstanceOf[String])
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[IllegalArgumentException] {
        require(l1.isInstanceOf[String])
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "scala.Predef.String"))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      require(l1.isInstanceOf[List[Int]])
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalArgumentException] {
        require(s1.isInstanceOf[List[Int]])
      }
      assert(e.getMessage == wasNotInstanceOf(s1, "scala.List"))
    }

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      require(date.isInstanceOf[Date])
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[IllegalArgumentException] {
        require(l1.isInstanceOf[Date])
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "java.util.Date"))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      require(!l1.isInstanceOf[String])
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[IllegalArgumentException] {
        require(!s1.isInstanceOf[String])
      }
      assert(e.getMessage == wasInstanceOf(s1, "scala.Predef.String"))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      require(!s1.isInstanceOf[List[Int]])
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalArgumentException] {
        require(!l1.isInstanceOf[List[Int]])
      }
      assert(e.getMessage == wasInstanceOf(l1, "scala.List"))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      require(!l1.isInstanceOf[Date])
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[IllegalArgumentException] {
        require(!date.isInstanceOf[Date])
      }
      assert(e.getMessage == wasInstanceOf(date, "java.util.Date"))
    }

  }

  describe("The require(boolean, clue) method") {

    val a = 3
    val b = 5

    it("should throw NullPointerException when null is passed in as clue") {
      val e = intercept[NullPointerException] {
        require(a == 3, null)
      }
      assert(e.getMessage == "clue was null")
    }

    it("should do nothing when is used to check a == 3") {
      require(a == 3, "dude")
    }

    it("should throw IllegalArgumentException when is used to check a == 5") {
      val e = intercept[IllegalArgumentException] {
        require(a == 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 5 == b") {
      require(5 == b, ", dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 == b") {
      val e = intercept[IllegalArgumentException] {
        require(3 == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, b) + ", dude")
    }

    it("should do nothing when is used to check a != 5") {
      require(a != 5, ". dude")
    }

    it("should throw IllegalArgumentException when is used to check a != 3") {
      val e = intercept[IllegalArgumentException] {
        require(a != 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 3 != b") {
      require(3 != b, "; dude")
    }

    it("should throw IllegalArgumentException when is used to check 5 != b") {
      val e = intercept[IllegalArgumentException] {
        require(5 != b, "; dude")
      }
      assert(e.getMessage == equaled(5, b) + "; dude")
    }

    it("should do nothing when is used to check 3 == 3") {
      require(3 == 3, "dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalArgumentException] {
        require(3 == 5, "dude")
      }
      assert(e.getMessage == "dude")
    }

    it("should throw IllegalArgumentException when is used to check a == b") {
      val e = intercept[IllegalArgumentException] {
        require(a == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(a, b) + ", dude")
    }

    it("should throw IllegalArgumentException when is used to check a == null") {
      val e = intercept[IllegalArgumentException] {
        require(a == null, ". dude")
      }
      assert(e.getMessage == didNotEqual(a, null) + ". dude")
    }

    it("should throw IllegalArgumentException when is used to check null == a") {
      val e = intercept[IllegalArgumentException] {
        require(null == a, "; dude")
      }
      assert(e.getMessage == didNotEqual(null, a) + "; dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 != a") {
      val e = intercept[IllegalArgumentException] {
        require(3 != a, ", dude")
      }
      assert(e.getMessage == equaled(3, a) + ", dude")
    }

    it("should do nothing when is used to check 5 != a") {
      require(5 != a, ". dude")
    }

    it("should do nothing when is used to check a === 3") {
      require(a === 3, "dude")
    }

    it("should throw IllegalArgumentException when is used to check a === 5") {
      val e = intercept[IllegalArgumentException] {
        require(a === 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 3 === a") {
      require(3 === a, ", dude")
    }

    it("should throw IllegalArgumentException when is used to check 5 === a") {
      val e = intercept[IllegalArgumentException] {
        require(5 === a, ", dude")
      }
      assert(e.getMessage == didNotEqual(5, a) + ", dude")
    }

    it("should do nothing when is used to check a !== 5") {
      require(a !== 5, ". dude")
    }

    it("should throw IllegalArgumentException when is used to check a !== 3") {
      val e = intercept[IllegalArgumentException] {
        require(a !== 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 5 !== a") {
      require(5 !== a, "; dude")
    }

    it("should throw IllegalArgumentException when is used to check 3 !== a") {
      val e = intercept[IllegalArgumentException] {
        require(3 !== a, "; dude")
      }
      assert(e.getMessage == equaled(3, a) + "; dude")
    }

    it("should do nothing when is used to check a > 2") {
      require(a > 2, ", dude")
    }

    it("should do nothing when is used to check 5 > a") {
      require(5 > a, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[IllegalArgumentException] {
        require(a > 3, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[IllegalArgumentException] {
        require(3 > a, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3) + ", dude")
    }

    it("should do nothing when is used to check a >= 3") {
      require(a >= 3, ", dude")
    }

    it("should do nothing when is used to check 3 >= a") {
      require(3 >= a, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[IllegalArgumentException] {
        require(a >= 4, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(3, 4) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[IllegalArgumentException] {
        require(2 >= a, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(2, 3) + ", dude")
    }

    it("should do nothing when is used to check b < 6") {
      require(b < 6, ", dude")
    }

    it("should do nothing when is used to check 3 < b") {
      require(3 < b, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[IllegalArgumentException] {
        require(b < 5, ", dude")
      }
      assert(e.getMessage == wasNotLessThan(5, 5) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[IllegalArgumentException] {
        require(5 < b, ", dude")
      }
      assert(e.getMessage == wasNotLessThan(5, 5) + ", dude")
    }

    it("should do nothing when is used to check b <= 5") {
      require(b <= 5, ", dude")
    }

    it("should do nothing when is used to check 5 <= b") {
      require(5 <= b, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[IllegalArgumentException] {
        require(b <= 4, ", dude")
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(5, 4) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[IllegalArgumentException] {
        require(6 <= b, ", dude")
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(6, 5) + ", dude")
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      require(a == 3 && b == 5, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 3 && b == 6, ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 && b == 5, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 && b == 6, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      require(a == 3 & b == 5, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 3 & b == 6, ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 & b == 5, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 & b == 6, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      require(a == 3 || b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      require(a == 3 || b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      require(a == 2 || b == 5, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 || b == 6, ", dude")
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      require(a == 3 | b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      require(a == 3 | b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      require(a == 2 | b == 5, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[IllegalArgumentException] {
        require(a == 2 | b == 6, ", dude")
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude")
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      require(a == 3 && (b == 5 && b > 3), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[IllegalArgumentException] {
        require(a == 3 && (b == 5 && b > 5), ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))) + ", dude")
    }

    it("should do nothing when is used to check !(a == 5)") {
      require(!(a == 5), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[IllegalArgumentException] {
        require(!(a == 3), ", dude")
      }
      assert(e.getMessage == equaled(3, 3) + ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[IllegalArgumentException] {
        require(a == 3 && !(b == 5), ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), equaled(5, 5)) + ", dude")
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      require((a == 3) == (b == 5), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[IllegalArgumentException] {
        require((a == 3) == (b != 5), ", dude")
      }
      assert(e.getMessage === didNotEqual(true, false) + ", dude")
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[IllegalArgumentException] {
        require(a == 5 && s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[IllegalArgumentException] {
        require(a == 5 & s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      require(a == 3 || s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      require(a == 3 | s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      require(a == 3 && { println("hi"); b == 5}, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[IllegalArgumentException] {
        require(a == 3 && { println("hi"); b == 3}, ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{\n  scala.this.Predef.println(\"hi\");\n  b.==(3)\n}")) + ", dude")
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      require({ println("hi"); b == 5} && a == 3, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[IllegalArgumentException] {
        require({ println("hi"); b == 5} && a == 5, ", dude")
      }
      assert(e.getMessage == commaBut(wasTrue("{\n  scala.this.Predef.println(\"hi\");\n  b.==(5)\n}"), didNotEqual(3, 5)) + ", dude")
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      require(neverRuns1(sys.error("Sad times 1")), "should not fail!")
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      require(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      require(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
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

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      require(s1 startsWith "hi", ", dude")
      require(s1.startsWith("hi"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[IllegalArgumentException] {
        require(s2 startsWith "hi", ", dude")
      }
      assert(e1.getMessage == didNotStartWith(s2, "hi") + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        require(s2.startsWith("hi"), ", dude")
      }
      assert(e2.getMessage == didNotStartWith(s2, "hi") + ", dude")
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      require(ci1 startsWith 1, ", dude")
      require(ci1.startsWith(1), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[IllegalArgumentException] {
        require(ci2 startsWith 1, ", dude")
      }
      assert(e1.getMessage == didNotStartWith(ci2, 1) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        require(ci2.startsWith(1), ", dude")
      }
      assert(e2.getMessage == didNotStartWith(ci2, 1) + ", dude")
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      require(!s2.startsWith("hi"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        require(!s1.startsWith("hi"), ", dude")
      }
      assert(e1.getMessage == startedWith(s1, "hi") + ", dude")
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      require(s2 endsWith "hi", ", dude")
      require(s2.endsWith("hi"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[IllegalArgumentException] {
        require(s1 endsWith "hi", ", dude")
      }
      assert(e1.getMessage == didNotEndWith(s1, "hi") + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        require(s1.endsWith("hi"), ", dude")
      }
      assert(e2.getMessage == didNotEndWith(s1, "hi") + ", dude")
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      require(ci2 endsWith 1, ", dude")
      require(ci2.endsWith(1), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[IllegalArgumentException] {
        require(ci1 endsWith 1, ", dude")
      }
      assert(e1.getMessage == didNotEndWith(ci1, 1) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        require(ci1.endsWith(1), ", dude")
      }
      assert(e2.getMessage == didNotEndWith(ci1, 1) + ", dude")
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      require(!s1.endsWith("hi"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        require(!s2.endsWith("hi"), ", dude")
      }
      assert(e1.getMessage == endedWith(s2, "hi") + ", dude")
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      require(s3 contains "hi", ", dude")
      require(s3.contains("hi"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[IllegalArgumentException] {
        require(s3 contains "hello", ", dude")
      }
      assert(e1.getMessage == didNotContain(s3, "hello") + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        require(s3.contains("hello"), ", dude")
      }
      assert(e2.getMessage == didNotContain(s3, "hello") + ", dude")
    }

    it("should do nothing when is used to check ci2 contains 2") {
      require(ci2 contains 2, ", dude")
      require(ci2.contains(2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        require(ci1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContain(ci1, 5) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        require(ci1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContain(ci1, 5) + ", dude")
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      require(!s3.contains("hello"), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[IllegalArgumentException] {
        require(!s3.contains("hi"), ", dude")
      }
      assert(e1.getMessage == contained(s3, "hi") + ", dude")
    }

    it("should do nothing when is used to check l1 contains 2") {
      require(l1 contains 2, ", dude")
      require(l1.contains(2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[IllegalArgumentException] {
        require(l1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContain(l1, 5) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        require(l1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContain(l1, 5) + ", dude")
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      require(ci1 eq ci3, ", dude")
      require(ci1.eq(ci3), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[IllegalArgumentException] {
        require(ci1 eq ci2, ", dude")
      }
      assert(e1.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        require(ci1.eq(ci2), ", dude")
      }
      assert(e2.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      require(!ci1.eq(ci2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[IllegalArgumentException] {
        require(!ci1.eq(ci3), ", dude")
      }
      assert(e.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      require(ci1 ne ci2, ", dude")
      require(ci1.ne(ci2), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[IllegalArgumentException] {
        require(ci1 ne ci3, ", dude")
      }
      assert(e1.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")

      val e2 = intercept[IllegalArgumentException] {
        require(ci1.ne(ci3), ", dude")
      }
      assert(e2.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      require(!ci1.ne(ci3), ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[IllegalArgumentException] {
        require(!ci1.ne(ci2), ", dude")
      }
      assert(e.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")
    }

    it("should do nothing when is used to check s4.isEmpty") {
      require(s4.isEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        require(s3.isEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(s3) + ", dude")
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      require(!s3.isEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        require(!s4.isEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(s4) + ", dude")
    }

    it("should do nothing when is used to check l2.isEmpty") {
      require(l2.isEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        require(l1.isEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(l1) + ", dude")
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      require(!l1.isEmpty, ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[IllegalArgumentException] {
        require(!l2.isEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(l2) + ", dude")
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      require(s1.isInstanceOf[String], ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[IllegalArgumentException] {
        require(l1.isInstanceOf[String], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "scala.Predef.String") + ", dude")
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      require(l1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalArgumentException] {
        require(s1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(s1, "scala.List") + ", dude")
    }

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      require(date.isInstanceOf[Date], ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[IllegalArgumentException] {
        require(l1.isInstanceOf[Date], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "java.util.Date") + ", dude")
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      require(!l1.isInstanceOf[String], ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[IllegalArgumentException] {
        require(!s1.isInstanceOf[String], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(s1, "scala.Predef.String") + ", dude")
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      require(!s1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalArgumentException] {
        require(!l1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(l1, "scala.List") + ", dude")
    }

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      require(!l1.isInstanceOf[Date], ", dude")
    }

    it("should throw IllegalArgumentException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[IllegalArgumentException] {
        require(!date.isInstanceOf[Date], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(date, "java.util.Date") + ", dude")
    }

  }

  describe("The requireState(boolean) method") {

    val a = 3
    val b = 5

    it("should do nothing when is used to check a == 3") {
      requireState(a == 3)
    }

    it("should throw IllegalStateException when is used to check a == 5") {
      val e = intercept[IllegalStateException] {
        requireState(a == 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 5 == b") {
      requireState(5 == b)
    }

    it("should throw IllegalStateException when is used to check 3 == b") {
      val e = intercept[IllegalStateException] {
        requireState(3 == b)
      }
      assert(e.getMessage == didNotEqual(3, b))
    }

    it("should do nothing when is used to check 3 == 3") {
      requireState(3 == 3)
    }

    it("should throw IllegalStateException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalStateException] {
        requireState(3 == 5)
      }
      assert(e.getMessage == "Expression was false")
    }

    it("should throw IllegalStateException when is used to check a == b") {
      val e = intercept[IllegalStateException] {
        requireState(a == b)
      }
      assert(e.getMessage == didNotEqual(a, b))
    }

    it("should throw IllegalStateException when is used to check a == null") {
      val e = intercept[IllegalStateException] {
        requireState(a == null)
      }
      assert(e.getMessage == didNotEqual(a, null))
    }

    it("should throw IllegalStateException when is used to check null == a") {
      val e = intercept[IllegalStateException] {
        requireState(null == a)
      }
      assert(e.getMessage == didNotEqual(null, a))
    }

    it("should do nothing when is used to check a === 3") {
      requireState(a === 3)
    }

    it("should throw IllegalStateException when is used to check a === 5") {
      val e = intercept[IllegalStateException] {
        requireState(a === 5)
      }
      assert(e.getMessage == didNotEqual(a, 5))
    }

    it("should do nothing when is used to check 3 === a") {
      requireState(3 === a)
    }

    it("should throw IllegalStateException when is used to check 5 === a") {
      val e = intercept[IllegalStateException] {
        requireState(5 === a)
      }
      assert(e.getMessage == didNotEqual(5, a))
    }

    it("should do nothing when is used to check a !== 5") {
      requireState(a !== 5)
    }

    it("should throw IllegalStateException when is used to check a !== 3") {
      val e = intercept[IllegalStateException] {
        requireState(a !== 3)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should do nothing when is used to check 5 !== a") {
      requireState(5 !== a)
    }

    it("should throw IllegalStateException when is used to check 3 !== a") {
      val e = intercept[IllegalStateException] {
        requireState(3 !== a)
      }
      assert(e.getMessage == equaled(a, 3))
    }

    it("should do nothing when is used to check a > 2") {
      requireState(a > 2)
    }

    it("should do nothing when is used to check 5 > a") {
      requireState(5 > a)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[IllegalStateException] {
        requireState(a > 3)
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[IllegalStateException] {
        requireState(3 > a)
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3))
    }

    it("should do nothing when is used to check a >= 3") {
      requireState(a >= 3)
    }

    it("should do nothing when is used to check 3 >= a") {
      requireState(3 >= a)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[IllegalStateException] {
        requireState(a >= 4)
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(3, 4))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[IllegalStateException] {
        requireState(2 >= a)
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(2, 3))
    }

    it("should do nothing when is used to check b < 6") {
      requireState(b < 6)
    }

    it("should do nothing when is used to check 3 < b") {
      requireState(3 < b)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[IllegalStateException] {
        requireState(b < 5)
      }
      assert(e.getMessage == wasNotLessThan(5, 5))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[IllegalStateException] {
        requireState(5 < b)
      }
      assert(e.getMessage == wasNotLessThan(5, 5))
    }

    it("should do nothing when is used to check b <= 5") {
      requireState(b <= 5)
    }

    it("should do nothing when is used to check 5 <= b") {
      requireState(5 <= b)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[IllegalStateException] {
        requireState(b <= 4)
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(5, 4))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[IllegalStateException] {
        requireState(6 <= b)
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(6, 5))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      requireState(a == 3 && b == 5)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 3 && b == 6)
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 && b == 5)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 && b == 6)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      requireState(a == 3 & b == 5)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 3 & b == 6)
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 & b == 5)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 & b == 6)
      }
      assert(e.getMessage == didNotEqual(3, 2))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      requireState(a == 3 || b == 5)
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      requireState(a == 3 || b == 6)
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      requireState(a == 2 || b == 5)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 || b == 6)
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      requireState(a == 3 | b == 5)
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      requireState(a == 3 | b == 6)
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      requireState(a == 2 | b == 5)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 | b == 6)
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      requireState(a == 3 && (b == 5 && b > 3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[IllegalStateException] {
        requireState(a == 3 && (b == 5 && b > 5))
      }
      assert(e.getMessage == commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))))
    }

    it("should do nothing when is used to check !(a == 5)") {
      requireState(!(a == 5))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[IllegalStateException] {
        requireState(!(a == 3))
      }
      assert(e.getMessage == equaled(3, 3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[IllegalStateException] {
        requireState(a == 3 && !(b == 5))
      }
      assert(e.getMessage == commaBut(equaled(3, 3), equaled(5, 5)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      requireState((a == 3) == (b == 5))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[IllegalStateException] {
        requireState((a == 3) == (b != 5))
      }
      assert(e.getMessage === didNotEqual(true, false))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[IllegalStateException] {
        requireState(a == 5 && s.changeState)
      }
      assert(s.state == false)
    }

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[IllegalStateException] {
        requireState(a == 5 & s.changeState)
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      requireState(a == 3 || s.changeState)
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      requireState(a == 3 | s.changeState)
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      requireState(a == 3 && { println("hi"); b == 5})
    }

    it("should throw IllegalStateException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[IllegalStateException] {
        requireState(a == 3 && { println("hi"); b == 3})
      }
      assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{\n  scala.this.Predef.println(\"hi\");\n  b.==(3)\n}")))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      requireState({ println("hi"); b == 5} && a == 3)
    }

    it("should throw IllegalStateException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[IllegalStateException] {
        requireState({ println("hi"); b == 5} && a == 5)
      }
      assert(e.getMessage == commaBut(wasTrue("{\n  scala.this.Predef.println(\"hi\");\n  b.==(5)\n}"), didNotEqual(3, 5)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      requireState(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      requireState(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      requireState(neverRuns3(sys.error("Sad times 3"))(0))
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

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      requireState(s1 startsWith "hi")
      requireState(s1.startsWith("hi"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[IllegalStateException] {
        requireState(s2 startsWith "hi")
      }
      assert(e1.getMessage == didNotStartWith(s2, "hi"))

      val e2 = intercept[IllegalStateException] {
        requireState(s2.startsWith("hi"))
      }
      assert(e2.getMessage == didNotStartWith(s2, "hi"))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      requireState(ci1 startsWith 1)
      requireState(ci1.startsWith(1))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[IllegalStateException] {
        requireState(ci2 startsWith 1)
      }
      assert(e1.getMessage == didNotStartWith(ci2, 1))

      val e2 = intercept[IllegalStateException] {
        requireState(ci2.startsWith(1))
      }
      assert(e2.getMessage == didNotStartWith(ci2, 1))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      requireState(!s2.startsWith("hi"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        requireState(!s1.startsWith("hi"))
      }
      assert(e1.getMessage == startedWith(s1, "hi"))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      requireState(s2 endsWith "hi")
      requireState(s2.endsWith("hi"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[IllegalStateException] {
        requireState(s1 endsWith "hi")
      }
      assert(e1.getMessage == didNotEndWith(s1, "hi"))

      val e2 = intercept[IllegalStateException] {
        requireState(s1.endsWith("hi"))
      }
      assert(e2.getMessage == didNotEndWith(s1, "hi"))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      requireState(ci2 endsWith 1)
      requireState(ci2.endsWith(1))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[IllegalStateException] {
        requireState(ci1 endsWith 1)
      }
      assert(e1.getMessage == didNotEndWith(ci1, 1))

      val e2 = intercept[IllegalStateException] {
        requireState(ci1.endsWith(1))
      }
      assert(e2.getMessage == didNotEndWith(ci1, 1))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      requireState(!s1.endsWith("hi"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        requireState(!s2.endsWith("hi"))
      }
      assert(e1.getMessage == endedWith(s2, "hi"))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      requireState(s3 contains "hi")
      requireState(s3.contains("hi"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[IllegalStateException] {
        requireState(s3 contains "hello")
      }
      assert(e1.getMessage == didNotContain(s3, "hello"))

      val e2 = intercept[IllegalStateException] {
        requireState(s3.contains("hello"))
      }
      assert(e2.getMessage == didNotContain(s3, "hello"))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      requireState(ci2 contains 2)
      requireState(ci2.contains(2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        requireState(ci1 contains 5)
      }
      assert(e1.getMessage == didNotContain(ci1, 5))

      val e2 = intercept[IllegalStateException] {
        requireState(ci1.contains(5))
      }
      assert(e2.getMessage == didNotContain(ci1, 5))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      requireState(!s3.contains("hello"))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        requireState(!s3.contains("hi"))
      }
      assert(e1.getMessage == contained(s3, "hi"))
    }

    it("should do nothing when is used to check l1 contains 2") {
      requireState(l1 contains 2)
      requireState(l1.contains(2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        requireState(l1 contains 5)
      }
      assert(e1.getMessage == didNotContain(l1, 5))

      val e2 = intercept[IllegalStateException] {
        requireState(l1.contains(5))
      }
      assert(e2.getMessage == didNotContain(l1, 5))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      requireState(ci1 eq ci3)
      requireState(ci1.eq(ci3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[IllegalStateException] {
        requireState(ci1 eq ci2)
      }
      assert(e1.getMessage == wasNotTheSameInstanceAs(ci1, ci2))

      val e2 = intercept[IllegalStateException] {
        requireState(ci1.eq(ci2))
      }
      assert(e2.getMessage == wasNotTheSameInstanceAs(ci1, ci2))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      requireState(!ci1.eq(ci2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[IllegalStateException] {
        requireState(!ci1.eq(ci3))
      }
      assert(e.getMessage == wasTheSameInstanceAs(ci1, ci3))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      requireState(ci1 ne ci2)
      requireState(ci1.ne(ci2))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[IllegalStateException] {
        requireState(ci1 ne ci3)
      }
      assert(e1.getMessage == wasTheSameInstanceAs(ci1, ci3))

      val e2 = intercept[IllegalStateException] {
        requireState(ci1.ne(ci3))
      }
      assert(e2.getMessage == wasTheSameInstanceAs(ci1, ci3))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      requireState(!ci1.ne(ci3))
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[IllegalStateException] {
        requireState(!ci1.ne(ci2))
      }
      assert(e.getMessage == wasNotTheSameInstanceAs(ci1, ci2))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      requireState(s4.isEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[IllegalStateException] {
        requireState(s3.isEmpty)
      }
      assert(e.getMessage == wasNotEmpty(s3))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      requireState(!s3.isEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[IllegalStateException] {
        requireState(!s4.isEmpty)
      }
      assert(e.getMessage == wasEmpty(s4))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      requireState(l2.isEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[IllegalStateException] {
        requireState(l1.isEmpty)
      }
      assert(e.getMessage == wasNotEmpty(l1))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      requireState(!l1.isEmpty)
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[IllegalStateException] {
        requireState(!l2.isEmpty)
      }
      assert(e.getMessage == wasEmpty(l2))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      requireState(s1.isInstanceOf[String])
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[IllegalStateException] {
        requireState(l1.isInstanceOf[String])
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "scala.Predef.String"))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      requireState(l1.isInstanceOf[List[Int]])
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalStateException] {
        requireState(s1.isInstanceOf[List[Int]])
      }
      assert(e.getMessage == wasNotInstanceOf(s1, "scala.List"))
    }

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      requireState(date.isInstanceOf[Date])
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[IllegalStateException] {
        requireState(l1.isInstanceOf[Date])
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "java.util.Date"))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      requireState(!l1.isInstanceOf[String])
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[IllegalStateException] {
        requireState(!s1.isInstanceOf[String])
      }
      assert(e.getMessage == wasInstanceOf(s1, "scala.Predef.String"))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      requireState(!s1.isInstanceOf[List[Int]])
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalStateException] {
        requireState(!l1.isInstanceOf[List[Int]])
      }
      assert(e.getMessage == wasInstanceOf(l1, "scala.List"))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      requireState(!l1.isInstanceOf[Date])
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[IllegalStateException] {
        requireState(!date.isInstanceOf[Date])
      }
      assert(e.getMessage == wasInstanceOf(date, "java.util.Date"))
    }

  }

  describe("The requireState(boolean, clue) method") {

    val a = 3
    val b = 5

    it("should throw NullPointerException when null is passed in as clue") {
      val e = intercept[NullPointerException] {
        requireState(a == 3, null)
      }
      assert(e.getMessage == "clue was null")
    }

    it("should do nothing when is used to check a == 3") {
      requireState(a == 3, "dude")
    }

    it("should throw IllegalStateException when is used to check a == 5") {
      val e = intercept[IllegalStateException] {
        requireState(a == 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 5 == b") {
      requireState(5 == b, ", dude")
    }

    it("should throw IllegalStateException when is used to check 3 == b") {
      val e = intercept[IllegalStateException] {
        requireState(3 == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, b) + ", dude")
    }

    it("should do nothing when is used to check a != 5") {
      requireState(a != 5, ". dude")
    }

    it("should throw IllegalStateException when is used to check a != 3") {
      val e = intercept[IllegalStateException] {
        requireState(a != 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 3 != b") {
      requireState(3 != b, "; dude")
    }

    it("should throw IllegalStateException when is used to check 5 != b") {
      val e = intercept[IllegalStateException] {
        requireState(5 != b, "; dude")
      }
      assert(e.getMessage == equaled(5, b) + "; dude")
    }

    it("should do nothing when is used to check 3 == 3") {
      requireState(3 == 3, "dude")
    }

    it("should throw IllegalStateException when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e = intercept[IllegalStateException] {
        requireState(3 == 5, "dude")
      }
      assert(e.getMessage == "dude")
    }

    it("should throw IllegalStateException when is used to check a == b") {
      val e = intercept[IllegalStateException] {
        requireState(a == b, ", dude")
      }
      assert(e.getMessage == didNotEqual(a, b) + ", dude")
    }

    it("should throw IllegalStateException when is used to check a == null") {
      val e = intercept[IllegalStateException] {
        requireState(a == null, ". dude")
      }
      assert(e.getMessage == didNotEqual(a, null) + ". dude")
    }

    it("should throw IllegalStateException when is used to check null == a") {
      val e = intercept[IllegalStateException] {
        requireState(null == a, "; dude")
      }
      assert(e.getMessage == didNotEqual(null, a) + "; dude")
    }

    it("should throw IllegalStateException when is used to check 3 != a") {
      val e = intercept[IllegalStateException] {
        requireState(3 != a, ", dude")
      }
      assert(e.getMessage == equaled(3, a) + ", dude")
    }

    it("should do nothing when is used to check 5 != a") {
      requireState(5 != a, ". dude")
    }

    it("should do nothing when is used to check a === 3") {
      requireState(a === 3, "dude")
    }

    it("should throw IllegalStateException when is used to check a === 5") {
      val e = intercept[IllegalStateException] {
        requireState(a === 5, "dude")
      }
      assert(e.getMessage == didNotEqual(a, 5) + " dude")
    }

    it("should do nothing when is used to check 3 === a") {
      requireState(3 === a, ", dude")
    }

    it("should throw IllegalStateException when is used to check 5 === a") {
      val e = intercept[IllegalStateException] {
        requireState(5 === a, ", dude")
      }
      assert(e.getMessage == didNotEqual(5, a) + ", dude")
    }

    it("should do nothing when is used to check a !== 5") {
      requireState(a !== 5, ". dude")
    }

    it("should throw IllegalStateException when is used to check a !== 3") {
      val e = intercept[IllegalStateException] {
        requireState(a !== 3, ". dude")
      }
      assert(e.getMessage == equaled(a, 3) + ". dude")
    }

    it("should do nothing when is used to check 5 !== a") {
      requireState(5 !== a, "; dude")
    }

    it("should throw IllegalStateException when is used to check 3 !== a") {
      val e = intercept[IllegalStateException] {
        requireState(3 !== a, "; dude")
      }
      assert(e.getMessage == equaled(3, a) + "; dude")
    }

    it("should do nothing when is used to check a > 2") {
      requireState(a > 2, ", dude")
    }

    it("should do nothing when is used to check 5 > a") {
      requireState(5 > a, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[IllegalStateException] {
        requireState(a > 3, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[IllegalStateException] {
        requireState(3 > a, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThan(3, 3) + ", dude")
    }

    it("should do nothing when is used to check a >= 3") {
      requireState(a >= 3, ", dude")
    }

    it("should do nothing when is used to check 3 >= a") {
      requireState(3 >= a, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[IllegalStateException] {
        requireState(a >= 4, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(3, 4) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[IllegalStateException] {
        requireState(2 >= a, ", dude")
      }
      assert(e.getMessage == wasNotGreaterThanOrEqualTo(2, 3) + ", dude")
    }

    it("should do nothing when is used to check b < 6") {
      requireState(b < 6, ", dude")
    }

    it("should do nothing when is used to check 3 < b") {
      requireState(3 < b, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[IllegalStateException] {
        requireState(b < 5, ", dude")
      }
      assert(e.getMessage == wasNotLessThan(5, 5) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[IllegalStateException] {
        requireState(5 < b, ", dude")
      }
      assert(e.getMessage == wasNotLessThan(5, 5) + ", dude")
    }

    it("should do nothing when is used to check b <= 5") {
      requireState(b <= 5, ", dude")
    }

    it("should do nothing when is used to check 5 <= b") {
      requireState(5 <= b, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[IllegalStateException] {
        requireState(b <= 4, ", dude")
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(5, 4) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[IllegalStateException] {
        requireState(6 <= b, ", dude")
      }
      assert(e.getMessage == wasNotLessThanOrEqualTo(6, 5) + ", dude")
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      requireState(a == 3 && b == 5, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 3 && b == 6, ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 && b == 5, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 && b == 6, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      requireState(a == 3 & b == 5, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 3 & b == 6, ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 & b == 5, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 & b == 6, ", dude")
      }
      assert(e.getMessage == didNotEqual(3, 2) + ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      requireState(a == 3 || b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      requireState(a == 3 || b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      requireState(a == 2 || b == 5, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 || b == 6, ", dude")
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      requireState(a == 3 | b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      requireState(a == 3 | b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      requireState(a == 2 | b == 5, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[IllegalStateException] {
        requireState(a == 2 | b == 6, ", dude")
      }
      assert(e.getMessage == commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude")
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      requireState(a == 3 && (b == 5 && b > 3), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[IllegalStateException] {
        requireState(a == 3 && (b == 5 && b > 5), ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))) + ", dude")
    }

    it("should do nothing when is used to check !(a == 5)") {
      requireState(!(a == 5), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[IllegalStateException] {
        requireState(!(a == 3), ", dude")
      }
      assert(e.getMessage == equaled(3, 3) + ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[IllegalStateException] {
        requireState(a == 3 && !(b == 5), ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), equaled(5, 5)) + ", dude")
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      requireState((a == 3) == (b == 5), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[IllegalStateException] {
        requireState((a == 3) == (b != 5), ", dude")
      }
      assert(e.getMessage === didNotEqual(true, false) + ", dude")
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[IllegalStateException] {
        requireState(a == 5 && s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[IllegalStateException] {
        requireState(a == 5 & s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      requireState(a == 3 || s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      requireState(a == 3 | s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      requireState(a == 3 && { println("hi"); b == 5}, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[IllegalStateException] {
        requireState(a == 3 && { println("hi"); b == 3}, ", dude")
      }
      assert(e.getMessage == commaBut(equaled(3, 3), wasFalse("{\n  scala.this.Predef.println(\"hi\");\n  b.==(3)\n}")) + ", dude")
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      requireState({ println("hi"); b == 5} && a == 3, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[IllegalStateException] {
        requireState({ println("hi"); b == 5} && a == 5, ", dude")
      }
      assert(e.getMessage == commaBut(wasTrue("{\n  scala.this.Predef.println(\"hi\");\n  b.==(5)\n}"), didNotEqual(3, 5)) + ", dude")
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      requireState(neverRuns1(sys.error("Sad times 1")), "should not fail!")
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      requireState(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      requireState(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
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

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      requireState(s1 startsWith "hi", ", dude")
      requireState(s1.startsWith("hi"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[IllegalStateException] {
        requireState(s2 startsWith "hi", ", dude")
      }
      assert(e1.getMessage == didNotStartWith(s2, "hi") + ", dude")

      val e2 = intercept[IllegalStateException] {
        requireState(s2.startsWith("hi"), ", dude")
      }
      assert(e2.getMessage == didNotStartWith(s2, "hi") + ", dude")
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      requireState(ci1 startsWith 1, ", dude")
      requireState(ci1.startsWith(1), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[IllegalStateException] {
        requireState(ci2 startsWith 1, ", dude")
      }
      assert(e1.getMessage == didNotStartWith(ci2, 1) + ", dude")

      val e2 = intercept[IllegalStateException] {
        requireState(ci2.startsWith(1), ", dude")
      }
      assert(e2.getMessage == didNotStartWith(ci2, 1) + ", dude")
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      requireState(!s2.startsWith("hi"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        requireState(!s1.startsWith("hi"), ", dude")
      }
      assert(e1.getMessage == startedWith(s1, "hi") + ", dude")
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      requireState(s2 endsWith "hi", ", dude")
      requireState(s2.endsWith("hi"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[IllegalStateException] {
        requireState(s1 endsWith "hi", ", dude")
      }
      assert(e1.getMessage == didNotEndWith(s1, "hi") + ", dude")

      val e2 = intercept[IllegalStateException] {
        requireState(s1.endsWith("hi"), ", dude")
      }
      assert(e2.getMessage == didNotEndWith(s1, "hi") + ", dude")
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      requireState(ci2 endsWith 1, ", dude")
      requireState(ci2.endsWith(1), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[IllegalStateException] {
        requireState(ci1 endsWith 1, ", dude")
      }
      assert(e1.getMessage == didNotEndWith(ci1, 1) + ", dude")

      val e2 = intercept[IllegalStateException] {
        requireState(ci1.endsWith(1), ", dude")
      }
      assert(e2.getMessage == didNotEndWith(ci1, 1) + ", dude")
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      requireState(!s1.endsWith("hi"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        requireState(!s2.endsWith("hi"), ", dude")
      }
      assert(e1.getMessage == endedWith(s2, "hi") + ", dude")
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      requireState(s3 contains "hi", ", dude")
      requireState(s3.contains("hi"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[IllegalStateException] {
        requireState(s3 contains "hello", ", dude")
      }
      assert(e1.getMessage == didNotContain(s3, "hello") + ", dude")

      val e2 = intercept[IllegalStateException] {
        requireState(s3.contains("hello"), ", dude")
      }
      assert(e2.getMessage == didNotContain(s3, "hello") + ", dude")
    }

    it("should do nothing when is used to check ci2 contains 2") {
      requireState(ci2 contains 2, ", dude")
      requireState(ci2.contains(2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        requireState(ci1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContain(ci1, 5) + ", dude")

      val e2 = intercept[IllegalStateException] {
        requireState(ci1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContain(ci1, 5) + ", dude")
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      requireState(!s3.contains("hello"), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[IllegalStateException] {
        requireState(!s3.contains("hi"), ", dude")
      }
      assert(e1.getMessage == contained(s3, "hi") + ", dude")
    }

    it("should do nothing when is used to check l1 contains 2") {
      requireState(l1 contains 2, ", dude")
      requireState(l1.contains(2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[IllegalStateException] {
        requireState(l1 contains 5, ", dude")
      }
      assert(e1.getMessage == didNotContain(l1, 5) + ", dude")

      val e2 = intercept[IllegalStateException] {
        requireState(l1.contains(5), ", dude")
      }
      assert(e2.getMessage == didNotContain(l1, 5) + ", dude")
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      requireState(ci1 eq ci3, ", dude")
      requireState(ci1.eq(ci3), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[IllegalStateException] {
        requireState(ci1 eq ci2, ", dude")
      }
      assert(e1.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")

      val e2 = intercept[IllegalStateException] {
        requireState(ci1.eq(ci2), ", dude")
      }
      assert(e2.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      requireState(!ci1.eq(ci2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[IllegalStateException] {
        requireState(!ci1.eq(ci3), ", dude")
      }
      assert(e.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      requireState(ci1 ne ci2, ", dude")
      requireState(ci1.ne(ci2), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[IllegalStateException] {
        requireState(ci1 ne ci3, ", dude")
      }
      assert(e1.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")

      val e2 = intercept[IllegalStateException] {
        requireState(ci1.ne(ci3), ", dude")
      }
      assert(e2.getMessage == wasTheSameInstanceAs(ci1, ci3) + ", dude")
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      requireState(!ci1.ne(ci3), ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[IllegalStateException] {
        requireState(!ci1.ne(ci2), ", dude")
      }
      assert(e.getMessage == wasNotTheSameInstanceAs(ci1, ci2) + ", dude")
    }

    it("should do nothing when is used to check s4.isEmpty") {
      requireState(s4.isEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[IllegalStateException] {
        requireState(s3.isEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(s3) + ", dude")
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      requireState(!s3.isEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[IllegalStateException] {
        requireState(!s4.isEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(s4) + ", dude")
    }

    it("should do nothing when is used to check l2.isEmpty") {
      requireState(l2.isEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[IllegalStateException] {
        requireState(l1.isEmpty, ", dude")
      }
      assert(e.getMessage == wasNotEmpty(l1) + ", dude")
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      requireState(!l1.isEmpty, ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[IllegalStateException] {
        requireState(!l2.isEmpty, ", dude")
      }
      assert(e.getMessage == wasEmpty(l2) + ", dude")
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      requireState(s1.isInstanceOf[String], ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[IllegalStateException] {
        requireState(l1.isInstanceOf[String], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "scala.Predef.String") + ", dude")
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      requireState(l1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalStateException] {
        requireState(s1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(s1, "scala.List") + ", dude")
    }

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      requireState(date.isInstanceOf[Date], ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[IllegalStateException] {
        requireState(l1.isInstanceOf[Date], ", dude")
      }
      assert(e.getMessage == wasNotInstanceOf(l1, "java.util.Date") + ", dude")
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      requireState(!l1.isInstanceOf[String], ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[IllegalStateException] {
        requireState(!s1.isInstanceOf[String], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(s1, "scala.Predef.String") + ", dude")
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      requireState(!s1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[IllegalStateException] {
        requireState(!l1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(l1, "scala.List") + ", dude")
    }

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      requireState(!l1.isInstanceOf[Date], ", dude")
    }

    it("should throw IllegalStateException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[IllegalStateException] {
        requireState(!date.isInstanceOf[Date], ", dude")
      }
      assert(e.getMessage == wasInstanceOf(date, "java.util.Date") + ", dude")
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
      requireNonNull(prefix, string, suffix)
    }

    it("should throw NullPointerException with correct message when one of passed parameters is null") {
      val e = intercept[NullPointerException] {
        requireNonNull(prefix, nullString, suffix)
      }
      assert(e.getMessage == "nullString was null")
    }

    it("should throw NullPointerException with correct message when two of passed parameters is null") {
      val e = intercept[NullPointerException] {
        requireNonNull(nullPrefix, nullString, suffix)
      }
      assert(e.getMessage == "nullPrefix and nullString were null")
    }

    it("should throw NullPointerException with correct message when three of passed parameters is null") {
      val e = intercept[NullPointerException] {
        requireNonNull(nullPrefix, nullString, nullSuffix)
      }
      assert(e.getMessage == "nullPrefix, nullString, and nullSuffix were null")
    }

    it("should throw NullPointerException with correct message when one of passed parameters through object property is null") {
      class AClass {
        val aNull: String = null
      }
      val aClass = new AClass
      val e = intercept[NullPointerException] {
        requireNonNull(prefix, aClass.aNull, suffix)
      }
      assert(e.getMessage == "aClass.aNull was null")
    }

    it("should throw NullPointerException with correct message when one of passed parameters through object method call is null") {
      class AClass {
        def returnNull: String = null
      }
      val aClass = new AClass
      val e = intercept[NullPointerException] {
        requireNonNull(prefix, aClass.returnNull, suffix)
      }
      assert(e.getMessage == "aClass.returnNull was null")
    }

    it("should throw NullPointerException with correct message when one of passed parameters through method call is null") {
      def returnNull: String = null
      val e = intercept[NullPointerException] {
        requireNonNull(prefix, returnNull, suffix)
      }
      assert(e.getMessage == "returnNull was null")
    }

  }

}
