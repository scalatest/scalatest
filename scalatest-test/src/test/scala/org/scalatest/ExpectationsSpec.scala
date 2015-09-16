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

import exceptions.TestFailedException
import OptionValues._
import org.scalatest.Fact.Unary_!

class ExpectationsSpec extends FunSpec with Expectations {

  val NEWLINE = scala.compat.Platform.EOL

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
      assert(fact.isLeaf == true)
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
      assert(fact.isLeaf == true)
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
      assert(fact.isLeaf == true)
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
      assert(fact.isLeaf == true)
      assert(fact.toString ==
        "No(" + NEWLINE +
        "  !Yes(expected 3, and got 3)" + NEWLINE +
        ")"
      )
    }
    it("should give a basic message for a binary expression with 2 leafs") {
      val fact1 = expectResult(3) { 3 } && expectResult(3) { 4 }
      assert(fact1.isNo)
      assert(fact1.factMessage  == "3 equaled 3, but 3 did not equal 4")
      assert(fact1.toString ==
        "No(" + NEWLINE +
        "  Yes(expected 3, and got 3) &&" + NEWLINE +
        "  No(expected 3, but got 4)" + NEWLINE +
        ")"
      )

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
    }
    it("should use vertical diagrammed style of message when one of component in a binary expression is not a leaf") {
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
              def someRandomMethod() {}
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
}
