/*
 * Copyright 2001-2015 Artima, Inc.
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
package expectations

import exceptions.TestFailedException
import OptionValues._
import org.scalatest.Fact.Unary_!
import org.scalatest.funspec.AnyFunSpec

import org.scalactic.Prettifier
import org.scalactic.exceptions.NullArgumentException

import java.util.Date

class ExpectationsSpec extends AnyFunSpec with Expectations {

  val NEWLINE = scala.compat.Platform.EOL

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

  class FloatLengthSize(value: Float) {
    val length: Float = value
    val size: Float = value
  }

  val floatLengthSize = new FloatLengthSize(2.0f)

  describe("The expectResult method") {
    it("should give a correct Fact result when the expectation fails") {
      val fact = expectResult(3) { 2 }
      assert(fact.isNo)
      assert(fact.factMessage  == "Expected 3, but got 2")
      assert(fact.simplifiedFactMessage == "3 did not equal 2")
      assert(fact.midSentenceFactMessage == "expected 3, but got 2")
      assert(fact.midSentenceSimplifiedFactMessage == "3 did not equal 2")
      assert(fact.rawFactMessage == "Expected {0}, but got {1}")
      assert(fact.rawSimplifiedFactMessage == "{0} did not equal {1}")
      assert(fact.rawMidSentenceFactMessage == "expected {0}, but got {1}")
      assert(fact.rawMidSentenceSimplifiedFactMessage == "{0} did not equal {1}")
      assert(fact.factMessageArgs == Vector(3, 2))
      assert(fact.simplifiedFactMessageArgs == Vector(3, 2))
      assert(fact.midSentenceFactMessageArgs == Vector(3, 2))
      assert(fact.midSentenceSimplifiedFactMessageArgs == Vector(3, 2))
      assert(fact.isLeaf)
      assert(!fact.isVacuousYes)
    }
    it("should give a correct Fact result when the expectation succeeds") {
      val fact = expectResult(3) { 3 }
      assert(fact.isYes)
      assert(fact.factMessage  == "Expected 3, and got 3")
      assert(fact.simplifiedFactMessage == "3 equaled 3")
      assert(fact.midSentenceFactMessage == "expected 3, and got 3")
      assert(fact.midSentenceSimplifiedFactMessage == "3 equaled 3")
      assert(fact.rawFactMessage == "Expected {0}, and got {1}")
      assert(fact.rawSimplifiedFactMessage == "{0} equaled {1}")
      assert(fact.rawMidSentenceFactMessage == "expected {0}, and got {1}")
      assert(fact.rawMidSentenceSimplifiedFactMessage == "{0} equaled {1}")
      assert(fact.factMessageArgs == Vector(3, 3))
      assert(fact.simplifiedFactMessageArgs == Vector(3, 3))
      assert(fact.midSentenceFactMessageArgs == Vector(3, 3))
      assert(fact.midSentenceSimplifiedFactMessageArgs == Vector(3, 3))
      assert(fact.isLeaf)
      assert(!fact.isVacuousYes)
    }
    it("should give a basic (non-diagrammed) error message for simple leaf nodes") {
      val fact = expectResult(3) { 4 }
      assert(fact.isNo)
      assert(fact.factMessage  == "Expected 3, but got 4")
      assert(fact.simplifiedFactMessage == "3 did not equal 4")
      assert(fact.midSentenceFactMessage == "expected 3, but got 4")
      assert(fact.midSentenceSimplifiedFactMessage == "3 did not equal 4")
      assert(fact.rawFactMessage == "Expected {0}, but got {1}")
      assert(fact.rawSimplifiedFactMessage == "{0} did not equal {1}")
      assert(fact.rawMidSentenceFactMessage == "expected {0}, but got {1}")
      assert(fact.rawMidSentenceSimplifiedFactMessage == "{0} did not equal {1}")
      assert(fact.factMessageArgs == Vector(3, 4))
      assert(fact.simplifiedFactMessageArgs == Vector(3, 4))
      assert(fact.midSentenceFactMessageArgs == Vector(3, 4))
      assert(fact.midSentenceSimplifiedFactMessageArgs == Vector(3, 4))
      assert(fact.isLeaf)
      assert(!fact.isVacuousYes)
      assert(fact.toString == "No(expected 3, but got 4)")
    }
    it("should still give a basic error message after negating a leaf with !") {
      val fact = !expectResult(3) { 3 }
      assert(fact.isNo)
      assert(fact.factMessage  == "3 equaled 3")
      assert(fact.simplifiedFactMessage == "3 equaled 3")
      assert(fact.midSentenceFactMessage == "3 equaled 3")
      assert(fact.midSentenceSimplifiedFactMessage == "3 equaled 3")
      assert(fact.rawFactMessage == "{0} equaled {1}")
      assert(fact.rawSimplifiedFactMessage == "{0} equaled {1}")
      assert(fact.rawMidSentenceFactMessage == "{0} equaled {1}")
      assert(fact.rawMidSentenceSimplifiedFactMessage == "{0} equaled {1}")
      assert(fact.factMessageArgs == Vector(3, 3))
      assert(fact.simplifiedFactMessageArgs == Vector(3, 3))
      assert(fact.midSentenceFactMessageArgs == Vector(3, 3))
      assert(fact.midSentenceSimplifiedFactMessageArgs == Vector(3, 3))
      assert(fact.isLeaf)
      assert(!fact.isVacuousYes)
      assert(fact.toString ==
        "No(" + NEWLINE +
        "  !Yes(expected 3, and got 3)" + NEWLINE +
        ")"
      )
    }
    it("should give a basic message for a && binary expression with 2 leafs") {
      val fact1 = expectResult(3) { 3 } && expectResult(3) { 4 }
      assert(fact1.isNo)
      assert(fact1.factMessage  == "3 equaled 3, but 3 did not equal 4")
      assert(fact1.toString ==
        "No(" + NEWLINE +
        "  Yes(expected 3, and got 3) &&" + NEWLINE +
        "  No(expected 3, but got 4)" + NEWLINE +
        ")"
      )
      assert(!fact1.isVacuousYes)

      val fact2 = expectResult(3) { 3 } && !expectResult(4) { 4 }
      assert(fact2.isNo)
      assert(fact2.factMessage  == "3 equaled 3, but 4 equaled 4")
      assert(fact2.toString ==
        "No(" + NEWLINE +
        "  Yes(expected 3, and got 3) &&" + NEWLINE +
        "  No(" + NEWLINE +
        "    !Yes(expected 4, and got 4)" + NEWLINE +
        "  )" + NEWLINE +
        ")"
      )
      assert(!fact2.isVacuousYes)
    }
    it("should give a basic message for a & binary expression with 2 leafs") {
      val fact1 = expectResult(3) { 3 } & expectResult(3) { 4 }
      assert(fact1.isNo)
      assert(fact1.factMessage  == "3 equaled 3, but 3 did not equal 4")
      assert(fact1.toString ==
        "No(" + NEWLINE +
          "  Yes(expected 3, and got 3) &" + NEWLINE +
          "  No(expected 3, but got 4)" + NEWLINE +
          ")"
      )
      assert(!fact1.isVacuousYes)

      val fact2 = expectResult(3) { 3 } & !expectResult(4) { 4 }
      assert(fact2.isNo)
      assert(fact2.factMessage  == "3 equaled 3, but 4 equaled 4")
      assert(fact2.toString ==
        "No(" + NEWLINE +
          "  Yes(expected 3, and got 3) &" + NEWLINE +
          "  No(" + NEWLINE +
          "    !Yes(expected 4, and got 4)" + NEWLINE +
          "  )" + NEWLINE +
          ")"
      )
      assert(!fact2.isVacuousYes)
    }
    it("should use vertical diagrammed style of message when one of component in && and || binary expression is not a leaf") {
      val fact = (expectResult(3) { 3 } && expectResult(3) { 4 }) || expectResult(5) { 6 }
      assert(fact.factMessage ==
        "No(" + NEWLINE +
        "  No(" + NEWLINE +
        "    Yes(expected 3, and got 3) &&" + NEWLINE +
        "    No(expected 3, but got 4)" + NEWLINE +
        "  ) ||" + NEWLINE +
        "  No(expected 5, but got 6)" + NEWLINE +
        ")"
      )
      assert(fact.toString ==
        "No(" + NEWLINE +
        "  No(" + NEWLINE +
        "    Yes(expected 3, and got 3) &&" + NEWLINE +
        "    No(expected 3, but got 4)" + NEWLINE +
        "  ) ||" + NEWLINE +
        "  No(expected 5, but got 6)" + NEWLINE +
        ")"
      )
      assert(!fact.isVacuousYes)
    }
    it("should use vertical diagrammed style of message when one of component in & and | binary expression is not a leaf") {
      val fact = (expectResult(3) { 3 } & expectResult(3) { 4 }) | expectResult(5) { 6 }
      assert(fact.factMessage ==
        "No(" + NEWLINE +
          "  No(" + NEWLINE +
          "    Yes(expected 3, and got 3) &" + NEWLINE +
          "    No(expected 3, but got 4)" + NEWLINE +
          "  ) |" + NEWLINE +
          "  No(expected 5, but got 6)" + NEWLINE +
          ")"
      )
      assert(fact.toString ==
        "No(" + NEWLINE +
          "  No(" + NEWLINE +
          "    Yes(expected 3, and got 3) &" + NEWLINE +
          "    No(expected 3, but got 4)" + NEWLINE +
          "  ) |" + NEWLINE +
          "  No(expected 5, but got 6)" + NEWLINE +
          ")"
      )
      assert(!fact.isVacuousYes)
    }
    it("should use vertical diagrammed style of message and prefix Unary_! instance with !") {
      val fact = (expectResult(3) { 3 } && !expectResult(4) { 4 }) || expectResult(5) { 6 }
      assert(fact.factMessage ==
        "No(" + NEWLINE +
        "  No(" + NEWLINE +
        "    Yes(expected 3, and got 3) &&" + NEWLINE +
        "    No(" + NEWLINE +
        "      !Yes(expected 4, and got 4)" + NEWLINE +
        "    )" + NEWLINE +
        "  ) ||" + NEWLINE +
        "  No(expected 5, but got 6)" + NEWLINE +
        ")"
      )
      assert(fact.toString ==
        "No(" + NEWLINE +
        "  No(" + NEWLINE +
        "    Yes(expected 3, and got 3) &&" + NEWLINE +
        "    No(" + NEWLINE +
        "      !Yes(expected 4, and got 4)" + NEWLINE +
        "    )" + NEWLINE +
        "  ) ||" + NEWLINE +
        "  No(expected 5, but got 6)" + NEWLINE +
        ")"
      )
      assert(!fact.isVacuousYes)
    }
    it("should short-circuit and just return No when used with No && No") {
      val fact = expectResult(2) {4} && expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isNo)
      assert(fact.factMessage == "Expected 2, but got 4")
      assert(!fact.isVacuousYes)
    }
    it("should short-circuit and just return No when used with No && Yes") {
      val fact = expectResult(2) {4} && expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isNo)
      assert(fact.factMessage == "Expected 2, but got 4")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_&& when used with Yes && No") {
      val fact = expectResult(4) {4} && expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_&&])
      assert(fact.factMessage == "4 equaled 4, but 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_&& when used with Yes && Yes") {
      val fact = expectResult(4) {4} && expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_&&])
      assert(fact.factMessage == "4 equaled 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_& when used with No & No") {
      val fact = expectResult(2) {4} & expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_&])
      assert(fact.factMessage == "2 did not equal 4, and 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_& when used with No & Yes") {
      val fact = expectResult(2) {4} & expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_&])
      assert(fact.factMessage == "2 did not equal 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_& when used with Yes & No") {
      val fact = expectResult(4) {4} & expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_&])
      assert(fact.factMessage == "4 equaled 4, but 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_& when used with Yes & Yes") {
      val fact = expectResult(4) {4} & expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_&])
      assert(fact.factMessage == "4 equaled 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_|| when used with No || No") {
      val fact = expectResult(2) {4} || expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_||])
      assert(fact.factMessage == "2 did not equal 4, and 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_|| when used with No || Yes") {
      val fact = expectResult(2) {4} || expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_||])
      assert(fact.factMessage == "2 did not equal 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
    it("should short-circuit and return Yes when used with Yes || No") {
      val fact = expectResult(4) {4} || expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isYes)
      assert(fact.factMessage == "Expected 4, and got 4")
      assert(!fact.isVacuousYes)
    }
    it("should short-circuit and return Yes when used with Yes || Yes") {
      val fact = expectResult(4) {4} || expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isYes)
      assert(fact.factMessage == "Expected 4, and got 4")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_| when used with No | No") {
      val fact = expectResult(2) {4} | expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_|])
      assert(fact.factMessage == "2 did not equal 4, and 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_| when used with No | Yes") {
      val fact = expectResult(2) {4} | expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_|])
      assert(fact.factMessage == "2 did not equal 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_| when used with Yes | No") {
      val fact = expectResult(4) {4} | expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_|])
      assert(fact.factMessage == "4 equaled 4, and 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_| when used with Yes | Yes") {
      val fact = expectResult(4) {4} | expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_|])
      assert(fact.factMessage == "4 equaled 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
  }

  describe("The expectThrows method") {
    it("should catch subtypes") {
      class MyException extends RuntimeException
      class MyExceptionSubClass extends MyException
      assert(
        (
          expectThrows[MyException] {
            throw new MyException
          } &&
          expectThrows[MyException] {
            throw new MyExceptionSubClass
          } && {
            // Try with a trait
            trait MyTrait {
              def someRandomMethod(): Unit = {}
            }
            class AnotherException extends RuntimeException with MyTrait
            expectThrows[MyTrait] {
              throw new AnotherException
            }
          }
        ).isYes
      )
    }
    it("should include the expected exception as the cause so that it will be there if negated") {
        val expectedException = new IllegalArgumentException("I meant to do that!")
        val fact =
          expectThrows[IllegalArgumentException] {
            throw expectedException
          }
        assert(fact.cause.value eq expectedException)
        assert((!fact).asInstanceOf[Fact.Unary_!].underlying.cause.value eq expectedException)
    }
    describe("when the bit of code throws the wrong exception") {
      it("should include that wrong exception as the Fact's cause") {
        val wrongException = new RuntimeException("oops!")
        val fact =
          expectThrows[IllegalArgumentException] {
            throw wrongException
          }
        assert(fact.cause.value eq wrongException)
      }
    }
  }

  describe("The expect(condition) method") {

    val x = 1

    it("should return Yes when used to check x == 1") {
      val fact = expect(x == 1)
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isYes)
      assert(fact.factMessage == "1 equaled 1")
      assert(!fact.isVacuousYes)
    }

    it("should return No when used to check x == 2") {
      val fact = expect(x == 2)
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isNo)
      assert(fact.factMessage == "1 did not equal 2")
      assert(!fact.isVacuousYes)
    }

    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should return Yes with correct message when is used to check a == 3") {
      val fact = expect(a == 3)
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3")
    }

    it("should return No with correct message when is used to check a == 5") {
      val fact = expect(a == 5)
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 5")
    }

    it("should return Yes with correct message when is used to check 5 == b") {
      val fact = expect(5 == b)
      assert(fact.isYes)
      assert(fact.factMessage == "5 equaled 5")
    }

    it("should return message with correct message when is used to check 3 == b") {
      val fact = expect(3 == b)
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 5")
    }

    it("should return Yes with correct message when is used to check a != 5") {
      val fact = expect(a != 5)
      assert(fact.isYes)
      assert(fact.factMessage == "3 did not equal 5")
    }

    it("should return No with correct message when is used to check a != 3") {
      val fact = expect(a != 3)
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3")
    }

    it("should return Yes with correct message when is used to check 3 != b") {
      val fact = expect(3 != b)
      assert(fact.isYes)
      assert(fact.factMessage == "3 did not equal 5")
    }

    it("should return No with correct message when is used to check 5 != b") {
      val fact = expect(5 != b)
      assert(fact.isNo)
      assert(fact.factMessage == "5 equaled 5")
    }

    it("should return Yes with correct message when is used to check 3 == 3") {
      val fact = expect(3 == 3)
      assert(fact.isYes)
      assert(fact.factMessage == "Expression was true")
    }

    it("should return No with message that contains the original code when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val fact1 = expect(3 == 5)
      assert(fact1.isNo)
      assert(fact1.factMessage == "Expression was false")
      
      val fact2 = expect(3 == 5, "3 did not equal 5")
      assert(fact2.isNo)
      assert(fact2.factMessage == "Expression was false 3 did not equal 5")
    }

    it("should return No with correct message when is used to check a == b") {
      val fact = expect(a == b)
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 5")
    }


    it("should return No with correct message when is used to check a == null") {
      val s = "test"
      val fact = expect(s == null)
      assert(fact.isNo)
      assert(fact.factMessage == "\"test\" did not equal null")
    }

    it("should return No with correct message when is used to check null == a") {
      val s = "test"
      val fact = expect(null == s)
      assert(fact.isNo)
      assert(fact.factMessage == "null did not equal \"test\"")
    }

    it("should return No with correct message when is used to check 3 != a") {
      val fact = expect(3 != a)
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3")
    }

    it("should return Yes with correct message when is used to check 5 != a") {
      val fact = expect(5 != a)
      assert(fact.isYes)
      assert(fact.factMessage == "5 did not equal 3")
    }

    it("should return Yes with correct message when is used to check a > 2") {
      val fact = expect(a > 2)
      assert(fact.isYes)
      assert(fact.factMessage == "3 was greater than 2")
    }

    it("should return Yes with correct message when is used to check 5 > a") {
      val fact = expect(5 > a)
      assert(fact.isYes)
      assert(fact.factMessage == "5 was greater than 3")
    }

    it("should return No with correct message when is used to check a > 3") {
      val fact = expect(a > 3)
      assert(fact.isNo)
      assert(fact.factMessage == "3 was not greater than 3")
    }

    it("should return No with correct message when is used to check 3 > a") {
      val fact = expect(3 > a)
      assert(fact.isNo)
      assert(fact.factMessage == "3 was not greater than 3")
    }

    it("should return Yes with correct message when is used to check a >= 3") {
      val fact = expect(a >= 3)
      assert(fact.isYes)
      assert(fact.factMessage == "3 was greater than or equal to 3")
    }

    it("should return Yes with correct message when is used to check 3 >= a") {
      val fact = expect(3 >= a)
      assert(fact.isYes)
      assert(fact.factMessage == "3 was greater than or equal to 3")
    }

    it("should return No with correct message when is used to check a >= 4") {
      val fact = expect(a >= 4)
      assert(fact.isNo)
      assert(fact.factMessage == "3 was not greater than or equal to 4")
    }

    it("should return No with correct message when is used to check 2 >= a") {
      val fact = expect(2 >= a)
      assert(fact.isNo)
      assert(fact.factMessage == "2 was not greater than or equal to 3")
    }

    it("should return Yes with correct message when is used to check b < 6") {
      val fact = expect(b < 6)
      assert(fact.isYes)
      assert(fact.factMessage == "5 was less than 6")
    }

    it("should return Yes with correct message when is used to check 3 < b") {
      val fact = expect(3 < b)
      assert(fact.isYes)
      assert(fact.factMessage == "3 was less than 5")
    }

    it("should return No with correct message when is used to check b < 5") {
      val fact = expect(b < 5)
      assert(fact.isNo)
      assert(fact.factMessage == "5 was not less than 5")
    }

    it("should return No with correct message when is used to check 5 < b") {
      val fact = expect(5 < b)
      assert(fact.isNo)
      assert(fact.factMessage == "5 was not less than 5")
    }

    it("should return Yes with correct message when is used to check b <= 5") {
      val fact = expect(b <= 5)
      assert(fact.isYes)
      assert(fact.factMessage == "5 was less than or equal to 5")
    }

    it("should return Yes with correct message when is used to check 5 <= b") {
      val fact = expect(5 <= b)
      assert(fact.isYes)
      assert(fact.factMessage == "5 was less than or equal to 5")
    }

    it("should return No with correct message when is used to check b <= 4") {
      val fact = expect(b <= 4)
      assert(fact.isNo)
      assert(fact.factMessage == "5 was not less than or equal to 4")
    }

    it("should return No with correct message when is used to check 6 <= b") {
      val fact = expect(6 <= b)
      assert(fact.isNo)
      assert(fact.factMessage == "6 was not less than or equal to 5")
    }

    it("should return No with correct message when is used to check java.lang.Integer(5) < java.lang.Integer(4) with import scala.math.Ordering.Implicits.infixOrderingOps in scope") {
      import scala.math.Ordering.Implicits.infixOrderingOps
      val first = new java.lang.Integer(5)
      val second = new java.lang.Integer(4)
      val fact = expect(first < second)
      assert(fact.isNo)
      assert(fact.factMessage == "5 was not less than 4")
    }

    it("should return Yes with correct message when is used to check bob == \"bob\"") {
      val fact = expect(bob == "bob")
      assert(fact.isYes)
      assert(fact.factMessage == "\"bob\" equaled \"bob\"")
    }

    it("should return Yes with correct message when is used to check bob != \"alice\"") {
      val fact = expect(bob != "alice")
      assert(fact.isYes)
      assert(fact.factMessage == "\"[bob]\" did not equal \"[alice]\"")
    }

    it("should return Yes with correct message when is used to check alice == \"alice\"") {
      val fact = expect(alice == "alice")
      assert(fact.isYes)
      assert(fact.factMessage == "\"alice\" equaled \"alice\"")
    }

    it("should return Yes with correct message when is used to check alice != \"bob\"") {
      val fact = expect(alice != "bob")
      assert(fact.isYes)
      assert(fact.factMessage == "\"[alice]\" did not equal \"[bob]\"")
    }

    it("should return No with correct message when is used to check bob == \"alice\"") {
      val fact = expect(bob == "alice")
      assert(fact.isNo)
      assert(fact.factMessage == "\"[bob]\" did not equal \"[alice]\"")
    }

    it("should return No with correct message when is used to check bob != \"bob\"") {
      val fact = expect(bob != "bob")
      assert(fact.isNo)
      assert(fact.factMessage == "\"bob\" equaled \"bob\"")
    }

    it("should return No with correct message when is used to check alice == \"bob\"") {
      val fact = expect(alice == "bob")
      assert(fact.isNo)
      assert(fact.factMessage == "\"[alice]\" did not equal \"[bob]\"")
    }

    it("should return No with correct message when is used to check alice != \"alice\"") {
      val fact = expect(alice != "alice")
      assert(fact.isNo)
      assert(fact.factMessage == "\"alice\" equaled \"alice\"")
    }

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalactic.TripleEquals does not seems to work
    // Should make Assertions to extend TripleEquals instead of LegacyTripleEquals instead.

    it("should return Yes with correct message when is used to check a === 3") {
      val fact = expect(a === 3)
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3")
    }

    it("should return No with correct message when is used to check a === 5") {
      val fact = expect(a === 5)
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 5")
    }

    it("should return Yes with correct message when is used to check 3 === a") {
      val fact = expect(3 === a)
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3")
    }

    it("should return No with correct message when is used to check 5 === a") {
      val fact = expect(5 === a)
      assert(fact.isNo)
      assert(fact.factMessage == "5 did not equal 3")
    }

    it("should return No with correct message when is used to check a !== 5") {
      val fact = expect(a !== 5)
      assert(fact.isYes)
      assert(fact.factMessage == "3 did not equal 5")
    }

  }

  describe("The expect(condition, clue) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should throw NullArgumentException when null is passed in as clue") {
      val e = intercept[NullArgumentException] {
        expect(a == 3, null)
      }
      assert(e.getMessage == "clue was null")
    }

    it("should return Yes with correct message") {
      val fact = expect(a == 3, "dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3 dude")
    }

    it("should return No with correct message when is used to check a == 5") {
      val fact = expect(a == 5, "dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 5 dude")
    }

    it("should return No when is used to check 5 == b") {
      val fact = expect(5 == b, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "5 equaled 5, dude")
    }

    it("should return No with correct message when is used to check 3 == b") {
      val fact = expect(3 == b, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 5, dude")
    }

    it("should return Yes with correct message when is used to check a != 5") {
      val fact = expect(a != 5, ". dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 did not equal 5. dude")
    }

    it("should return No with correct message when is used to check a != 3") {
      val fact = expect(a != 3, ". dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3. dude")
    }

    it("should return Yes with correct message when is used to check 3 != b") {
      val fact = expect(3 != b, "; dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 did not equal 5; dude")
    }

    it("should return No with correct message when is used to check 5 != b") {
      val fact = expect(5 != b, "; dude")
      assert(fact.isNo)
      assert(fact.factMessage == "5 equaled 5; dude")
    }

    it("should return Yes with correct message when is used to check 3 == 3") {
      val fact = expect(3 == 3, "dude")
      assert(fact.isYes)
      assert(fact.factMessage == "Expression was true dude")
    }

    it("should return No with correct message when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val f1 = expect(3 == 5, "dude")
      assert(f1.isNo)
      assert(f1.factMessage === "Expression was false dude")

      val f2 = expect(3 == 5, "3 did not equal 5")
      assert(f2.isNo)
      assert(f2.factMessage === "Expression was false 3 did not equal 5")
    }

    it("should return No with correct message when is used to check a == b") {
      val fact = expect(a == b, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 5, dude")
    }

    it("should return No with correct message when is used to check a == null") {
      val s = "test"
      val fact = expect(s == null, ". dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"test\" did not equal null. dude")
    }

    it("should return No with correct message when is used to check null == a") {
      val s = "test"
      val fact = expect(null == s, "; dude")
      assert(fact.isNo)
      assert(fact.factMessage == "null did not equal \"test\"; dude")
    }

    it("should return No with correct message when is used to check 3 != a") {
      val fact = expect(3 != a, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3, dude")
    }

    it("should return Yes with correct message when is used to check 5 != a") {
      val fact = expect(5 != a, ". dude")
      assert(fact.isYes)
      assert(fact.factMessage == "5 did not equal 3. dude")
    }

    it("should return Yes with correct message when is used to check a > 2") {
      val fact = expect(a > 2, ". dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 was greater than 2. dude")
    }

    it("should return Yes with correct message when is used to check 5 > a") {
      val fact = expect(5 > a, ". dude")
      assert(fact.isYes)
      assert(fact.factMessage == "5 was greater than 3. dude")
    }

    it("should return No with correct message when is used to check a > 3") {
      val fact = expect(a > 3, ". dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 was not greater than 3. dude")
    }

    it("should return No with correct message when is used to check 3 > a") {
      val fact = expect(3 > a, "; dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 was not greater than 3; dude")
    }

    it("should return Yes with correct message when is used to check a >= 3") {
      val fact = expect(a >= 3, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 was greater than or equal to 3, dude")
    }

    it("should return Yes with correct message when is used to check 3 >= a") {
      val fact = expect(3 >= a, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 was greater than or equal to 3, dude")
    }

    it("should return No with correct message when is used to check a >= 4") {
      val fact = expect(a >= 4, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 was not greater than or equal to 4, dude")
    }

    it("should return No with correct message when is used to check 2 >= a") {
      val fact = expect(2 >= a, ". dude")
      assert(fact.isNo)
      assert(fact.factMessage == "2 was not greater than or equal to 3. dude")
    }

    it("should return Yes with correct message when is used to check b < 6") {
      val fact = expect(b < 6, "; dude")
      assert(fact.isYes)
      assert(fact.factMessage == "5 was less than 6; dude")
    }

    it("should return Yes with correct message when is used to check 3 < b") {
      val fact = expect(3 < b, "; dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 was less than 5; dude")
    }

    it("should return No with correct message when is used to check b < 5") {
      val fact = expect(b < 5, "; dude")
      assert(fact.isNo)
      assert(fact.factMessage == "5 was not less than 5; dude")
    }

    it("should return No with correct message when is used to check 5 < b") {
      val fact = expect(5 < b, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "5 was not less than 5, dude")
    }

    it("should return Yes with correct message when is used to check b <= 5") {
      val fact = expect(b <= 5, ". dude")
      assert(fact.isYes)
      assert(fact.factMessage == "5 was less than or equal to 5. dude")
    }

    it("should return Yes with correct message when is used to check 5 <= b") {
      val fact = expect(5 <= b, ". dude")
      assert(fact.isYes)
      assert(fact.factMessage == "5 was less than or equal to 5. dude")
    }

    it("should return No with correct message when is used to check b <= 4") {
      val fact = expect(b <= 4, ". dude")
      assert(fact.isNo)
      assert(fact.factMessage == "5 was not less than or equal to 4. dude")
    }

    it("should return No with correct message when is used to check 6 <= b") {
      val fact = expect(6 <= b, "; dude")
      assert(fact.isNo)
      assert(fact.factMessage == "6 was not less than or equal to 5; dude")
    }

    it("should return No with correct message when is used to check java.lang.Integer(5) < java.lang.Integer(4) with import scala.math.Ordering.Implicits.infixOrderingOps in scope") {
      import scala.math.Ordering.Implicits.infixOrderingOps
      val first = new java.lang.Integer(5)
      val second = new java.lang.Integer(4)
      val fact = expect(first < second, "; dude")
      assert(fact.isNo)
      assert(fact.factMessage == "5 was not less than 4; dude")
    }

    it("should return Yes with correct message when is used to check bob == \"bob\"") {
      val fact = expect(bob == "bob", "dude")
      assert(fact.isYes)
      assert(fact.factMessage == "\"bob\" equaled \"bob\" dude")
    }

    it("should return Yes with correct message when is used to check bob != \"alice\"") {
      val fact = expect(bob != "alice", "dude")
      assert(fact.isYes)
      assert(fact.factMessage == "\"[bob]\" did not equal \"[alice]\" dude")
    }

    it("should return Yes with correct message when is used to check alice == \"alice\"") {
      val fact = expect(alice == "alice", "dude")
      assert(fact.isYes)
      assert(fact.factMessage == "\"alice\" equaled \"alice\" dude")
    }

    it("should return Yes with correct message when is used to check alice != \"bob\"") {
      val fact = expect(alice != "bob", "dude")
      assert(fact.isYes)
      assert(fact.factMessage == "\"[alice]\" did not equal \"[bob]\" dude")
    }

    it("should return No with correct message when is used to check bob == \"alice\"") {
      val fact = expect(bob == "alice", "dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"[bob]\" did not equal \"[alice]\" dude")
    }

    it("should return No with correct message when is used to check bob != \"bob\"") {
      val fact = expect(bob != "bob", ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"bob\" equaled \"bob\", dude")
    }

    it("should return No with correct message when is used to check alice == \"bob\"") {
      val fact = expect(alice == "bob", ". dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"[alice]\" did not equal \"[bob]\". dude")
    }

    it("should return No with correct message when is used to check alice != \"alice\"") {
      val fact = expect(alice != "alice", "; dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"alice\" equaled \"alice\"; dude")
    }

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalactic.TripleEquals does not seems to work
    // Should make Assertions to extend TripleEquals instead of LegacyTripleEquals instead.

    it("should return Yes with correct message when is used to check a === 3") {
      val fact = expect(a === 3, "dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3 dude")
    }

    it("should return No with correct message when is used to check a === 5") {
      val fact = expect(a === 5, "dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 5 dude")
    }

    it("should return Yes with correct message when is used to check 3 === a") {
      val fact = expect(3 === a, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3, dude")
    }

    it("should return No with correct message when is used to check 5 === a") {
      val fact = expect(5 === a, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "5 did not equal 3, dude")
    }

    it("should return Yes with correct message when is used to check a !== 5") {
      val fact = expect(a !== 5, ". dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 did not equal 5. dude")
    }

    it("should return No with correct message when is used to check a !== 3") {
      val fact = expect(a !== 3, ". dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3. dude")
    }

    it("should return Yes with correct message when is used to check 5 !== a") {
      val fact = expect(5 !== a, "; dude")
      assert(fact.isYes)
      assert(fact.factMessage == "5 did not equal 3; dude")
    }

    it("should return No with correct message when is used to check 3 !== a") {
      val fact = expect(3 !== a, "; dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3; dude")
    }

    it("should return Yes with correct message when is used to check a == 3 && b == 5") {
      val fact = expect(a == 3 && b == 5, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3, and 5 equaled 5, dude")
    }

    it("should return No with correct message when is used to check a == 3 && b == 6") {
      val fact = expect(a == 3 && b == 6, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3, but 5 did not equal 6, dude")
    }

    it("should return No with correct message when is used to check a == 2 && b == 5") {
      val fact = expect(a == 2 && b == 5, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 2, dude")
    }

    it("should return No with correct message when is used to check a == 2 && b == 6") {
      val fact = expect(a == 2 && b == 6, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 2, dude")
    }

    it("should return No with correct message when is used to check a == 3 & b == 5") {
      val fact = expect(a == 3 & b == 5, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3, and 5 equaled 5, dude")
    }

    it("should return No with correct message when is used to check a == 3 & b == 6") {
      val fact = expect(a == 3 & b == 6, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3, but 5 did not equal 6, dude")
    }

    it("should return No with correct message when is used to check a == 2 & b == 5") {
      val fact = expect(a == 2 & b == 5, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 2, but 5 equaled 5, dude")
    }

    it("should return No with correct message when is used to check a == 2 & b == 6") {
      val fact = expect(a == 2 & b == 6, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 2, and 5 did not equal 6, dude")
    }

    it("should return Yes with correct message when is used to check a == 3 || b == 5") {
      val fact = expect(a == 3 || b == 5, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3, dude")
    }

    it("should return Yes with correct message when is used to check a == 3 || b == 6") {
      val fact = expect(a == 3 || b == 6, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3, dude")
    }

    it("should return Yes with correct message when is used to check a == 2 || b == 5") {
      val fact = expect(a == 2 || b == 5, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 did not equal 2, but 5 equaled 5, dude")
    }

    it("should return No with correct message when is used to check a == 2 || b == 6") {
      val fact = expect(a == 2 || b == 6, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 2, and 5 did not equal 6, dude")
    }

    it("should return Yes with correct message when is used to check a == 3 | b == 5") {
      val fact = expect(a == 3 | b == 5, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3, and 5 equaled 5, dude")
    }

    it("should return Yes with correct message when is used to check a == 3 | b == 6") {
      val fact = expect(a == 3 | b == 6, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3, but 5 did not equal 6, dude")
    }

    it("should return Yes with correct message when is used to check a == 2 | b == 5") {
      val fact = expect(a == 2 | b == 5, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 did not equal 2, but 5 equaled 5, dude")
    }

    it("should return No with correct message when is used to check a == 2 | b == 6") {
      val fact = expect(a == 2 | b == 6, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 did not equal 2, and 5 did not equal 6, dude")
    }

    it("should return Yes with correct message when is used to check a == 3 && (b == 5 && b > 3)") {
      val fact = expect(a == 3 && (b == 5 && b > 3), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 equaled 3, and 5 equaled 5, and 5 was greater than 3, dude")
    }

    it("should return No with correct message when is used to check a == 3 && (b == 5 && b > 5)") {
      val fact = expect(a == 3 && (b == 5 && b > 5), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3, but 5 equaled 5, but 5 was not greater than 5, dude")
    }

    it("should return Yes with correct message when is used to check !(a == 5)") {
      val fact = expect(!(a == 5), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "3 did not equal 5, dude")
    }

    it("should return No with correct message when is used to check !(a == 3)") {
      val fact = expect(!(a == 3), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3, dude")
    }

    it("should return No with correct message when is used to check a == 3 && !(b == 5)") {
      val fact = expect(a == 3 && !(b == 5), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "3 equaled 3, but 5 equaled 5, dude")
    }

    it("should return Yes with correct message when is used to check (a == 3) == (b == 5)") {
      val fact = expect((a == 3) == (b == 5), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "true equaled true, dude")
    }

    it("should return No with correct message when is used to check (a == 3) == (b != 5)") {
      val fact = expect((a == 3) == (b != 5), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "true did not equal false, dude")
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      val fact = expect(a == 5 && s.changeState, ", dude")
      assert(fact.isNo)
      assert(s.state == false)
    }

    it("should not short-circuit & when first condition was false") {
      val s = new Stateful
      val fact = expect(a == 5 & s.changeState, ", dude")
      assert(fact.isNo)
      assert(s.state == true)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      val fact = expect(a == 3 || s.changeState, ", dude")
      assert(fact.isYes)
      assert(s.state == false)
    }

    it("should not short-circuit | when first condition was true") {
      val s = new Stateful
      val fact = expect(a == 3 | s.changeState, ", dude")
      assert(fact.isYes)
      assert(s.state == true)
    }

    it("should return Yes with correct message when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      val fact = expect(a == 3 && { println("hi"); b == 5}, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage.startsWith("3 equaled 3, and "))
      assert(fact.factMessage.endsWith(" was true, dude"))
    }

    it("should return No with correct message when is used to check a == 3 && { println(\"hi\"); b == 3}") {
      val fact = expect(a == 3 && { println("hi"); b == 3}, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage.startsWith("3 equaled 3, but "))
      assert(fact.factMessage.endsWith(" was false, dude"))
    }

    it("should return Yes with correct message when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      val fact = expect({ println("hi"); b == 5} && a == 3, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage.endsWith("was true, and 3 equaled 3, dude"))
    }

    it("should return No with correct message when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val fact = expect({ println("hi"); b == 5} && a == 5, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage.endsWith(" was true, but 3 did not equal 5, dude"))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      val fact = expect(neverRuns1(sys.error("Sad times 1")), "should not fail!")
      assert(fact.isYes)
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      val fact = expect(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
      assert(fact.isYes)
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      val fact = expect(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
      assert(fact.isYes)
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

    it("should return Yes with correct message when is used to check s1 startsWith \"hi\"") {
      val fact = expect(s1 startsWith "hi", ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "\"hi ScalaTest\" started with \"hi\", dude")

      val fact2 = expect(s1.startsWith("hi"), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == "\"hi ScalaTest\" started with \"hi\", dude")
    }

    it("should return No with correct message when is used to check s2 startsWith \"hi\"") {
      val fact = expect(s2 startsWith "hi", ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"ScalaTest hi\" did not start with \"hi\", dude")
      
      val fact2 = expect(s2.startsWith("hi"), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == "\"ScalaTest hi\" did not start with \"hi\", dude")
    }

    it("should return Yes with correct message when is used to check ci1 startsWith 1") {
      val fact = expect(ci1 startsWith 1, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "123 started with 1, dude")
      
      val fact2 = expect(ci1.startsWith(1), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == "123 started with 1, dude")
    }

    it("should return No with correct message when is used to check ci2 startsWith 1") {
      val fact = expect(ci2 startsWith 1, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "321 did not start with 1, dude")

      val fact2 = expect(ci2.startsWith(1), ", dude")
      assert(fact2.isNo)
      assert(fact.factMessage == "321 did not start with 1, dude")
    }

    it("should return Yes with correct message when is used to check !s2.startsWith(\"hi\")") {
      val fact = expect(!s2.startsWith("hi"), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "\"ScalaTest hi\" did not start with \"hi\", dude")
    }

    it("should return No with correct message when is used to check !s1.startsWith(\"hi\")") {
      val fact = expect(!s1.startsWith("hi"), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"hi ScalaTest\" started with \"hi\", dude")
    }

    it("should return Yes with correct message when is used to check s2 endsWith \"hi\"") {
      val fact = expect(s2 endsWith "hi", ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "\"ScalaTest hi\" ended with \"hi\", dude")
      
      val fact2 = expect(s2.endsWith("hi"), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == "\"ScalaTest hi\" ended with \"hi\", dude")
    }

    it("should return No with correct message when is used to check s1 endsWith \"hi\"") {
      val fact = expect(s1 endsWith "hi", ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"hi ScalaTest\" did not end with \"hi\", dude")

      val fact2 = expect(s1.endsWith("hi"), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == "\"hi ScalaTest\" did not end with \"hi\", dude")
    }

    it("should return Yes with correct message when is used to check ci2 endsWith 1") {
      val fact = expect(ci2 endsWith 1, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "321 ended with 1, dude")
      
      val fact2 = expect(ci2.endsWith(1), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == "321 ended with 1, dude")
    }

    it("should return No with correct message when is used to check ci1 endsWith 1") {
      val fact = expect(ci1 endsWith 1, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "123 did not end with 1, dude")

      val fact2 = expect(ci1.endsWith(1), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == "123 did not end with 1, dude")
    }

    it("should return Yes when is used to check !s1.endsWith(\"hi\")") {
      val fact = expect(!s1.endsWith("hi"), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "\"hi ScalaTest\" did not end with \"hi\", dude")
    }

    it("should return No with correct message when is used to check !s2.endsWith(\"hi\")") {
      val fact = expect(!s2.endsWith("hi"), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"ScalaTest hi\" ended with \"hi\", dude")
    }

    it("should return Yes with correct message when is used to check s3 contains \"hi\"") {
      val fact = expect(s3 contains "hi", ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "\"Say hi to ScalaTest\" contained \"hi\", dude")
      
      val fact2 = expect(s3.contains("hi"), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == "\"Say hi to ScalaTest\" contained \"hi\", dude")
    }

    it("should return No with correct message when is used to check s3 contains \"hello\"") {
      val fact = expect(s3 contains "hello", ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"Say hi to ScalaTest\" did not contain \"hello\", dude")

      val fact2 = expect(s3.contains("hello"), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == "\"Say hi to ScalaTest\" did not contain \"hello\", dude")
    }

    it("should return Yes with correct message when is used to check ci2 contains 2") {
      val fact = expect(ci2 contains 2, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "321 contained 2, dude")
      
      val fact2 = expect(ci2.contains(2), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == "321 contained 2, dude")
    }

    it("should return No with correct message when is used to check ci1 contains 5") {
      val fact = expect(ci1 contains 5, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "123 did not contain 5, dude")

      val fact2 = expect(ci1.contains(5), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == "123 did not contain 5, dude")
    }

    it("should return Yes with correct message when is used to check !s1.contains(\"hello\")") {
      val fact = expect(!s3.contains("hello"), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "\"Say hi to ScalaTest\" did not contain \"hello\", dude")
    }

    it("should return No with correct message when is used to check !s3.contains(\"hi\")") {
      val fact = expect(!s3.contains("hi"), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"Say hi to ScalaTest\" contained \"hi\", dude")
    }

    it("should return Yes when is used to check l1 contains 2") {
      val fact = expect(l1 contains 2, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "List(1, 2, 3) contained 2, dude")
      
      val fact2 = expect(l1.contains(2), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == "List(1, 2, 3) contained 2, dude")
    }

    it("should return No with correct message when is used to check l1 contains 5") {
      val fact = expect(l1 contains 5, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "List(1, 2, 3) did not contain 5, dude")
      
      val fact2 = expect(l1.contains(5), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == "List(1, 2, 3) did not contain 5, dude")
    }

    it("should return Yes when is used to check !(l1 contains 5)") {
      val fact = expect(!(l1 contains 5), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == "List(1, 2, 3) did not contain 5, dude")
      
      val fact2 = expect(!l1.contains(5), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == "List(1, 2, 3) did not contain 5, dude")
    }

    it("should return No with correct message when is used to check !(l1 contains 2)") {
      val fact = expect(!(l1 contains 2), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "List(1, 2, 3) contained 2, dude")
      
      val fact2 = expect(!l1.contains(2), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == "List(1, 2, 3) contained 2, dude")
    }

    it("should return Yes with correct message when is used to check m1 contains 2") {
      val fact = expect(m1 contains 2, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(m1) + " contained key 2, dude")

      val fact2 = expect(m1.contains(2), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == Prettifier.default(m1) + " contained key 2, dude")
    }

    it("should return No with correct message when is used to check m1 contains 5") {
      val fact = expect(m1 contains 5, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(m1) + " did not contain key 5, dude")

      val fact2 = expect(m1.contains(5), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == Prettifier.default(m1) + " did not contain key 5, dude")
    }

    it("should return Yes with correct message when is used to check !(m1 contains 5)") {
      val fact = expect(!(m1 contains 5), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(m1) + " did not contain key 5, dude")

      val fact2 = expect(!m1.contains(5), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == Prettifier.default(m1) + " did not contain key 5, dude")
    }

    it("should return No with correct message when is used to check !(m1 contains 2)") {
      val fact = expect(!(m1 contains 2), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(m1) + " contained key 2, dude")

      val fact2 = expect(!m1.contains(2), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == Prettifier.default(m1) + " contained key 2, dude")
    }

    it("should return Yes when is used to check ct1 contains 8") {
      val fact = expect(ct1 contains 8, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(ct1) + " contained 8, dude")
      
      val fact2 = expect(ct1.contains(8), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == Prettifier.default(ct1) + " contained 8, dude")
    }

    it("should return No with correct message when is used to check ct1 contains 5") {
      val fact = expect(ct1 contains 5, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(ct1) + " did not contain 5, dude")

      val fact2 = expect(ct1.contains(5), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == Prettifier.default(ct1) + " did not contain 5, dude")
    }

    it("should return Yes with correct message when is used to check !ct1.contains(5)") {
      val fact = expect(!ct1.contains(5), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(ct1) + " did not contain 5, dude")
    }

    it("should return No with correct message when is used to check !ct1.contains(8)") {
      val fact = expect(!ct1.contains(8), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(ct1) + " contained 8, dude")
    }

    it("should return Yes with correct message when is used to check ci1 eq ci3") {
      val fact = expect(ci1 eq ci3, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(ci1) + " was the same instance as " + Prettifier.default(ci3) + ", dude")
      
      val fact2 = expect(ci1.eq(ci3), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == Prettifier.default(ci1) + " was the same instance as " + Prettifier.default(ci3) + ", dude")
    }

    it("should return No with correct message and stack depth when is used to check ci1 eq ci2") {
      val fact = expect(ci1 eq ci2, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(ci1) + " was not the same instance as " + Prettifier.default(ci2) + ", dude")

      val fact2 = expect(ci1.eq(ci2), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == Prettifier.default(ci1) + " was not the same instance as " + Prettifier.default(ci2) + ", dude")
    }

    it("should return Yes with correct message when is used to check !ci1.eq(ci2)") {
      val fact = expect(!ci1.eq(ci2), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(ci1) + " was not the same instance as " + Prettifier.default(ci2) + ", dude")
    }

    it("should return No with correct message when is used to check !ci1.eq(ci3)") {
      val fact = expect(!ci1.eq(ci3), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(ci1) + " was the same instance as " + Prettifier.default(ci3) + ", dude")
    }

    it("should return Yes with correct message when is used to check ci1 ne ci2") {
      val fact = expect(ci1 ne ci2, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(ci1) + " was not the same instance as " + Prettifier.default(ci2) + ", dude")

      val fact2 = expect(ci1.ne(ci2), ", dude")
      assert(fact2.isYes)
      assert(fact2.factMessage == Prettifier.default(ci1) + " was not the same instance as " + Prettifier.default(ci2) + ", dude")
    }

    it("should return No with correct message when is used to check ci1 ne ci3") {
      val fact = expect(ci1 ne ci3, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(ci1) + " was the same instance as " + Prettifier.default(ci3) + ", dude")

      val fact2 = expect(ci1.ne(ci3), ", dude")
      assert(fact2.isNo)
      assert(fact2.factMessage == Prettifier.default(ci1) + " was the same instance as " + Prettifier.default(ci3) + ", dude")
    }

    it("should return Yes with correct message when is used to check !ci1.ne(ci3)") {
      val fact = expect(!ci1.ne(ci3), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(ci1) + " was the same instance as " + Prettifier.default(ci3) + ", dude")
    }

    it("should return No with correct message when is used to check !ci1.ne(ci2)") {
      val fact = expect(!ci1.ne(ci2), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(ci1) + " was not the same instance as " + Prettifier.default(ci2) + ", dude")
    }

    it("should return Yes with correct message when is used to check s4.isEmpty") {
      val fact = expect(s4.isEmpty, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(s4) + " was empty, dude")
    }

    it("should return No with correct message and stack depth when is used to check s3.isEmpty") {
      val fact = expect(s3.isEmpty, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(s3) + " was not empty, dude")
    }

    it("should return Yes when is used to check !s3.isEmpty") {
      val fact = expect(!s3.isEmpty, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(s3) + " was not empty, dude")
    }

    it("should return No with correct message when is used to check !s4.isEmpty") {
      val fact = expect(!s4.isEmpty, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(s4) + " was empty, dude")
    }

    it("should return Yes with correct message when is used to check l2.isEmpty") {
      val fact = expect(l2.isEmpty, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l2) + " was empty, dude")
    }

    it("should return No with correct message when is used to check l1.isEmpty") {
      val fact = expect(l1.isEmpty, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " was not empty, dude")
    }

    it("should return Yes with correct message when is used to check !l1.isEmpty") {
      val fact = expect(!l1.isEmpty, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " was not empty, dude")
    }

    it("should return No with correct message when is used to check !l2.isEmpty") {
      val fact = expect(!l2.isEmpty, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l2) + " was empty, dude")
    }

    it("should return Yes when is used to check s3.nonEmpty") {
      val fact = expect(s3.nonEmpty, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(s3) + " was not empty, dude")
    }

    it("should return No with correct message when is used to check s4.nonEmpty") {
      val fact = expect(s4.nonEmpty, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(s4) + " was empty, dude")
    }

    it("should return Yes with correct message when is used to check !s4.nonEmpty") {
      val fact = expect(!s4.nonEmpty, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(s4) + " was empty, dude")
    }

    it("should return No with correct message when is used to check !s3.nonEmpty") {
      val fact = expect(!s3.nonEmpty, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(s3) + " was not empty, dude")
    }

    it("should return Yes with correct message when is used to check l1.nonEmpty") {
      val fact = expect(l1.nonEmpty, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " was not empty, dude")
    }

    it("should return No with correct message when is used to check l2.nonEmpty") {
      val fact = expect(l2.nonEmpty, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l2) + " was empty, dude")
    }

    it("should return Yes when is used to check !l2.nonEmpty") {
      val fact = expect(!l2.nonEmpty, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l2) + " was empty, dude")
    }

    it("should return No with correct message when is used to check !l1.nonEmpty") {
      val fact = expect(!l1.nonEmpty, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " was not empty, dude")
    }

    it("should return Yes with correct message when is used to check s1.isInstanceOf[String]") {
      val fact = expect(s1.isInstanceOf[String], ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(s1) + " was instance of scala.Predef.String, dude")
    }

    it("should return No with correct message when is used to check l1.isInstanceOf[String]") {
      val fact = expect(l1.isInstanceOf[String], ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " was not instance of scala.Predef.String, dude")
    }

    it("should return Yes with correct message when is used to check l1.isInstanceOf[List[Int]]") {
      val fact = expect(l1.isInstanceOf[List[Int]], ", dude")
      assert(fact.isYes)
      if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
        assert(fact.factMessage == Prettifier.default(l1) + " was instance of scala.collection.immutable.List[scala.Int], dude")
      else
        assert(fact.factMessage == Prettifier.default(l1) + " was instance of scala.List, dude")
    }

    it("should return No with correct message when is used to check s1.isInstanceOf[List[Int]]") {
      val fact = expect(s1.isInstanceOf[List[Int]], ", dude")
      assert(fact.isNo)
      if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
        assert(fact.factMessage == Prettifier.default(s1) + " was not instance of scala.collection.immutable.List[scala.Int], dude")
      else
        assert(fact.factMessage == Prettifier.default(s1) + " was not instance of scala.List, dude")
    }

    it("should return Yes with correct message when is used to check date.isInstanceOf[Date]") {
      val fact = expect(date.isInstanceOf[Date], ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(date) + " was instance of java.util.Date, dude")
    }

    it("should return No with correct message when is used to check l1.isInstanceOf[Date]") {
      val fact = expect(l1.isInstanceOf[Date], ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " was not instance of java.util.Date, dude")
    }

    it("should return Yes with correct message when is used to check !l1.isInstanceOf[String]") {
      val fact = expect(!l1.isInstanceOf[String], ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " was not instance of scala.Predef.String, dude")
    }

    it("should return No with correct message when is used to check !s1.isInstanceOf[String]") {
      val fact = expect(!s1.isInstanceOf[String], ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(s1) + " was instance of scala.Predef.String, dude")
    }

    it("should return Yes with correct message when is used to check !s1.isInstanceOf[List[Int]]") {
      val fact = expect(!s1.isInstanceOf[List[Int]], ", dude")
      assert(fact.isYes)
      if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
        assert(fact.factMessage == Prettifier.default(s1) + " was not instance of scala.collection.immutable.List[scala.Int], dude")
      else
        assert(fact.factMessage == Prettifier.default(s1) + " was not instance of scala.List, dude")
    }

    it("should return No with correct message when is used to check !l1.isInstanceOf[List[Int]]") {
      val fact = expect(!l1.isInstanceOf[List[Int]], ", dude")
      assert(fact.isNo)
      if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
        assert(fact.factMessage == Prettifier.default(l1) + " was instance of scala.collection.immutable.List[scala.Int], dude")
      else
        assert(fact.factMessage == Prettifier.default(l1) + " was instance of scala.List, dude")
    }

    it("should return Yes with correct message when is used to check !l1.isInstanceOf[Date]") {
      val fact = expect(!l1.isInstanceOf[Date], ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " was not instance of java.util.Date, dude")
    }

    it("should return No with correct message when is used to check !date.isInstanceOf[Date]") {
      val fact = expect(!date.isInstanceOf[Date], ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(date) + " was instance of java.util.Date, dude")
    }

    it("should return Yes with correct message when is used to check s1.length == 9") {
      val fact = expect(s1.length == 12, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(s1) + " had length 12, dude")
    }

    it("should return No with correct message when is used to check s1.length == 10") {
      val fact = expect(s1.length == 10, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(s1) + " had length 12 instead of expected length 10, dude")
    }

    it("should return Yes with correct message when is used to check l1.length == 3") {
      val fact = expect(l1.length == 3, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " had length 3, dude")
    }

    it("should return No with correct message when is used to check l1.length == 10") {
      val fact = expect(l1.length == 10, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " had length 3 instead of expected length 10, dude")
    }

    it("should return Yes with correct message when is used to check !(s1.length == 10)") {
      val fact = expect(!(s1.length == 10), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(s1) + " had length 12 instead of expected length 10, dude")
    }

    it("should return No with correct message when is used to check !(s1.length == 9)") {
      val fact = expect(!(s1.length == 12), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(s1) + " had length 12, dude")
    }

    it("should return Yes with correct message when is used to check !(l1.length == 2)") {
      val fact = expect(!(l1.length == 2), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " had length 3 instead of expected length 2, dude")
    }

    it("should return No with correct message when is used to check !(l1.length == 9)") {
      val fact = expect(!(l1.length == 3), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " had length 3, dude")
    }

    it("should return Yes with correct message when is used to check floatLengthSize.length == 2.0f") {
      val fact = expect(floatLengthSize.length == 2.0f, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(floatLengthSize) + " had length " + Prettifier.default(2.0f) + ", dude")
    }

    it("should return No with correct message when is used to check floatLengthSize.length == 1.0f") {
      val fact = expect(floatLengthSize.length == 1.0f, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(floatLengthSize) + " had length " + Prettifier.default(2.0f) + " instead of expected length " + Prettifier.default(1.0f) + ", dude")
    }

    it("should return Yes with correct message when is used to check s1.size == 9") {
      val fact = expect(s1.size == 12, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(s1) + " had size 12, dude")
    }

    it("should return No with correct message when is used to check s1.size == 10") {
      val fact = expect(s1.size == 10, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(s1) + " had size 12 instead of expected size 10, dude")
    }

    it("should return Yes with correct message when is used to check l1.size == 3") {
      val fact = expect(l1.size == 3, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " had size 3, dude")
    }

    it("should return No with correct message when is used to check l1.size == 10") {
      val fact = expect(l1.size == 10, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " had size 3 instead of expected size 10, dude")
    }

    it("should return Yes with correct message when is used to check !(s1.size == 10)") {
      val fact = expect(!(s1.size == 10), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(s1) + " had size 12 instead of expected size 10, dude")
    }

    it("should return No with correct message when is used to check !(s1.size == 9)") {
      val fact = expect(!(s1.size == 12), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(s1) + " had size 12, dude")
    }

    it("should return Yes with correct message when is used to check !(l1.size == 2)") {
      val fact = expect(!(l1.size == 2), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " had size 3 instead of expected size 2, dude")
    }

    it("should return Yes with correct message when is used to check floatLengthSize.size == 2.0f") {
      val fact = expect(floatLengthSize.size == 2.0f, ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(floatLengthSize) + " had size " + Prettifier.default(2.0f) + ", dude")
    }

    it("should return No with correct message when is used to check floatLengthSize.size == 1.0f") {
      val fact = expect(floatLengthSize.size == 1.0f, ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(floatLengthSize) + " had size " + Prettifier.default(2.0f) + " instead of expected size " + Prettifier.default(1.0f) + ", dude")
    }

    it("should return No with correct message when is used to check !(l1.size == 9)") {
      val fact = expect(!(l1.size == 3), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " had size 3, dude")
    }

    it("should return Yes with correct message when is used to check l1.exists(_ == 3)") {
      val fact = expect(l1.exists(_ == 3), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " contained 3, dude")
    }

    it("should return Yes with correct message when is used to check l1.exists(3 == _)") {
      val fact = expect(l1.exists(3 == _), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " contained 3, dude")
    }

    it("should return No with correct message when is used to check l1.exists(_ == 5)") {
      val fact = expect(l1.exists(_ == 5), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " did not contain 5, dude")
    }

    it("should return No with correct message when is used to check l1.exists(5 == _)") {
      val fact = expect(l1.exists(5 == _), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " did not contain 5, dude")
    }

    it("should return Yes with correct message when is used to check !l1.exists(_ == 5)") {
      val fact = expect(!l1.exists(_ == 5), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " did not contain 5, dude")
    }

    it("should return Yes with correct message when is used to check !l1.exists(5 == _)") {
      val fact = expect(!l1.exists(5 == _), ", dude")
      assert(fact.isYes)
      assert(fact.factMessage == Prettifier.default(l1) + " did not contain 5, dude")
    }

    it("should return No with correct message when is used to check !l1.exists(_ == 3)") {
      val fact = expect(!l1.exists(_ == 3), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " contained 3, dude")
    }

    it("should return No with correct message when is used to check !l1.exists(3 == _)") {
      val fact = expect(!l1.exists(3 == _), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == Prettifier.default(l1) + " contained 3, dude")
    }

    it("should return No with correct message when is used to check l1.exists(_ > 3)") {
      val fact = expect(l1.exists(_ > 3), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage endsWith " was false, dude")
    }

    it("should return No with correct message when is used to check l1.exists(3 < _)") {
      val fact = expect(l1.exists(3 < _), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage endsWith " was false, dude")
    }

    it("should return No with correct message when is used to check l3.exists(_.isEmpty)") {
      val fact = expect(l3.exists(_.isEmpty), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage endsWith " was false, dude")
    }

    it("should return No with correct message when is used to check l3.exists(false)") {
      val fact = expect(ci1.exists(321), ", dude")
      assert(fact.isNo)
      assert(fact.factMessage endsWith " was false, dude")
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should return Yes when used to check woof { meow(y = 5) } == \"woof\"") {
      val fact = expect(woof { meow(y = 5) } == "woof", ", dude")
      assert(fact.isYes)
      assert(fact.factMessage endsWith "\"woof\" equaled \"woof\", dude")
    }

    it("should return No with correct message when is used to check woof { meow(y = 5) } == \"meow\"") {
      val fact = expect(woof { meow(y = 5) } == "meow", ", dude")
      assert(fact.isNo)
      assert(fact.factMessage == "\"[woof]\" did not equal \"[meow]\", dude")
    }
  }

  describe("expectDoesNotCompile method") {

    describe("when work with string literal") {

      it("should return Yes with correct fact message when type check failed") {
        val fact = expectDoesNotCompile("val a: String = 1")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.didNotCompile("val a: String = 1"))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when parse and type check passed") {
        val fact = expectDoesNotCompile("val a = 1")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        assert(fact.factMessage == Resources.expectedCompileErrorButGotNone("val a = 1"))
        assert(!fact.isVacuousYes)
      }

      it("should return Yes with correct fact messsage when parse failed") {
        val fact = expectDoesNotCompile("println(\"test)")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.didNotCompile("println(\"test)"))
        assert(!fact.isVacuousYes)
      }

      it("should do nothing when used with 'val i: Int = null'") {
        val fact = expectDoesNotCompile("val i: Int = null")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
      }

      it("should return No with correct fact message when the code compiles with implicit view in scope ") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val fact = expectDoesNotCompile("arrayList.asScala".stripMargin)

        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        assert(fact.factMessage == Resources.expectedCompileErrorButGotNone("arrayList.asScala"))
        assert(!fact.isVacuousYes)
      }
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should return Yes with correct fact message when type check failed") {
        val fact = expectDoesNotCompile(
          """
            |val a: String = 2
            |""".stripMargin
        )
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.didNotCompile(
          """
            |val a: String = 2
            |""".stripMargin
        ))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when parse and type check passed") {
        val fact = expectDoesNotCompile(
          """
            |val a = 1
            |""".stripMargin
        )
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        assert(fact.factMessage == Resources.expectedCompileErrorButGotNone(
          """
            |val a = 1
            |""".stripMargin
        ))
        assert(!fact.isVacuousYes)
      }

      it("should return Yes with correct fact message when parse failed ") {
        val fact = expectDoesNotCompile(
          """
            |println(\"test)
            |""".stripMargin
        )
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.didNotCompile(
          """
            |println(\"test)
            |""".stripMargin
        ))
        assert(!fact.isVacuousYes)
      }

      it("should throw TestFailedException with correct message and stack depth when the code compiles with implicit view in scope ") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val fact =
          expectDoesNotCompile(
            """
              |arrayList.asScala
              |""".stripMargin)

        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        assert(fact.factMessage == Resources.expectedCompileErrorButGotNone(NEWLINE + "arrayList.asScala" + NEWLINE))
        assert(!fact.isVacuousYes)
      }
    }
  }

  describe("expectCompiles method") {

    describe("when work with string literal") {

      it("should return Yes with correct fact message when type check passed") {
        val fact = expectCompiles("val a = 1")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.compiledSuccessfully("val a = 1"))
        assert(!fact.isVacuousYes)
      }

      
      it("should return No with correct fact message when type check failed") {
        val fact = expectCompiles("val a: String = 2")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
          assert(fact.factMessage == Resources.expectedNoErrorButGotTypeError(
            """Found:    (2 : Int)
              |Required: String
              |
              |The following import might fix the problem:
              |
              |  import org.scalactic.Prettifier.default
              |
              |""".stripMargin, "val a: String = 2"))
        else    
          assert(fact.factMessage == Resources.expectedNoErrorButGotTypeError(
            """type mismatch;
              | found   : Int(2)
              | required: String""".stripMargin, "val a: String = 2"))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when parse failed") {
        val fact = expectCompiles("println(\"test)")
        if (ScalaTestVersions.BuiltForScalaVersion == "2.10")
          assert(fact.factMessage == Resources.expectedNoErrorButGotParseError("reflective compilation has failed: \n\nunclosed string literal\n')' expected but eof found.", "println(\"test)"))
        else if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
          assert(fact.factMessage == Resources.expectedNoErrorButGotParseError("expression expected but erroneous token found", "println(\"test)"))
        else
          assert(fact.factMessage == Resources.expectedNoErrorButGotParseError("unclosed string literal", "println(\"test)"))

        assert(!fact.isVacuousYes)
      }
      
      it("should return Yes with correct fact message when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val fact = expectCompiles("arrayList.asScala")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.compiledSuccessfully("arrayList.asScala"))
        assert(!fact.isVacuousYes)
      }
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should return Fact with correct fact message when type check passed") {
        val fact =
          expectCompiles(
            """
              |val a = 1
              |""".stripMargin
          )
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.compiledSuccessfully(
          """
            |val a = 1
            |""".stripMargin
        ))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when type check failed") {
        val fact =
          expectCompiles(
            """
              |val a: String = 2
              |""".stripMargin
          )
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
          assert(fact.factMessage == Resources.expectedNoErrorButGotTypeError(
            """Found:    (2 : Int)
              |Required: String
              |
              |The following import might fix the problem:
              |
              |  import org.scalactic.Prettifier.default
              |
              |""".stripMargin, 
            """
              |val a: String = 2
              |""".stripMargin))
        else
          assert(fact.factMessage == Resources.expectedNoErrorButGotTypeError(
            """type mismatch;
              | found   : Int(2)
              | required: String""".stripMargin,
            """
              |val a: String = 2
              |""".stripMargin))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when parse failed") {
        val fact =
          expectCompiles(
            """
              |println("test)
              |""".stripMargin
          )
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        if (ScalaTestVersions.BuiltForScalaVersion == "2.10")
          assert(fact.factMessage == Resources.expectedNoErrorButGotParseError("reflective compilation has failed: \n\nunclosed string literal\n')' expected but '}' found.", "\nprintln(\"test)\n"))
        else if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
          assert(fact.factMessage == Resources.expectedNoErrorButGotParseError(
            "expression expected but erroneous token found",
            """
              |println("test)
              |""".stripMargin
          ))
        else
          assert(fact.factMessage == Resources.expectedNoErrorButGotParseError(
            "unclosed string literal",
            """
              |println("test)
              |""".stripMargin
          ))
        assert(!fact.isVacuousYes)
      }

      it("should return Yes with correct fact message when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val fact =
          expectCompiles(
            """
              |arrayList.asScala
              |""".stripMargin)

        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.compiledSuccessfully(NEWLINE + "arrayList.asScala" + NEWLINE))
        assert(!fact.isVacuousYes)
      }
    }
  }

  describe("expectTypeError method ") {

    describe("when work with string literal") {

      it("should return Yes with correct fact message when type check failed") {
        val fact = expectTypeError("val a: String = 1")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.gotTypeErrorAsExpected("val a: String = 1"))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when type check passed") {
        val fact = expectTypeError("val a = 1")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        assert(fact.factMessage == Resources.expectedTypeErrorButGotNone("val a = 1"))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when parse failed ") {
        val fact = expectTypeError("println(\"test)")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        if (ScalaTestVersions.BuiltForScalaVersion == "2.10")
          assert(fact.factMessage == Resources.expectedTypeErrorButGotParseError("reflective compilation has failed: \n\nunclosed string literal\n')' expected but eof found.", "println(\"test)"))
        else if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
          assert(fact.factMessage == Resources.expectedTypeErrorButGotParseError("expression expected but erroneous token found", "println(\"test)"))
        else
          assert(fact.factMessage == Resources.expectedTypeErrorButGotParseError("unclosed string literal", "println(\"test)"))
        assert(!fact.isVacuousYes)
      }
      
      it("should return Yes with correct fact message when used with 'val i: Int = null'") {
        val fact = expectTypeError("val i: Int = null")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.gotTypeErrorAsExpected("val i: Int = null"))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val fact = expectTypeError("arrayList.asScala")

        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        assert(fact.factMessage == Resources.expectedTypeErrorButGotNone("arrayList.asScala"))
        assert(!fact.isVacuousYes)
      }
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should return Yes with correct fact message when type check failed") {
        val fact =
          expectTypeError(
            """
              |val a: String = 2
              |""".stripMargin
          )
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.gotTypeErrorAsExpected(
          """
            |val a: String = 2
            |""".stripMargin
        ))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when type check passed") {
        val fact =
          expectTypeError(
            """
              |val a = 1
              |""".stripMargin
          )
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        assert(fact.factMessage == Resources.expectedTypeErrorButGotNone(
          """
            |val a = 1
            |""".stripMargin
        ))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when parse failed ") {
        val fact =
          expectTypeError(
            """
              |println("test)
              |""".stripMargin
          )

        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        if (ScalaTestVersions.BuiltForScalaVersion == "2.10")
          assert(fact.factMessage == Resources.expectedTypeErrorButGotParseError("reflective compilation has failed: \n\nunclosed string literal\n')' expected but '}' found.", "\nprintln(\"test)\n"))
        else if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
          assert(fact.factMessage == Resources.expectedTypeErrorButGotParseError(
            "expression expected but erroneous token found",
            """
              |println("test)
              |""".stripMargin
          ))
        else
          assert(fact.factMessage == Resources.expectedTypeErrorButGotParseError(
            "unclosed string literal",
            """
              |println("test)
              |""".stripMargin
          ))
        assert(!fact.isVacuousYes)
      }

      it("should return Yes with correct fact message when used with 'val i: Int = null'") {
        val fact =
          expectTypeError(
            """
              |val i: Int = null
              |""".stripMargin)
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.gotTypeErrorAsExpected(NEWLINE + "val i: Int = null" + NEWLINE))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val fact =
          expectTypeError(
            """
              |arrayList.asScala
              |""".stripMargin)

        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        assert(fact.factMessage == Resources.expectedTypeErrorButGotNone(NEWLINE + "arrayList.asScala" + NEWLINE))
        assert(!fact.isVacuousYes)
      }
    }
  }
}
