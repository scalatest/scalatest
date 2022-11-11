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

class DirectExpectationsSpec extends AnyFunSpec {

  val NEWLINE = scala.compat.Platform.EOL

  describe("The expectResult method") {
    it("should give a correct Fact result when the expectation fails") {
      val fact = org.scalatest.expectations.Expectations.expectResult(3) { 2 } 
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
      val fact = org.scalatest.expectations.Expectations.expectResult(3) { 3 } 
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
      val fact = org.scalatest.expectations.Expectations.expectResult(3) { 4 }
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
      val fact = !org.scalatest.expectations.Expectations.expectResult(3) { 3 }
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
      val fact1 = org.scalatest.expectations.Expectations.expectResult(3) { 3 } && org.scalatest.expectations.Expectations.expectResult(3) { 4 }
      assert(fact1.isNo)
      assert(fact1.factMessage  == "3 equaled 3, but 3 did not equal 4")
      assert(fact1.toString ==
        "No(" + NEWLINE +
        "  Yes(expected 3, and got 3) &&" + NEWLINE +
        "  No(expected 3, but got 4)" + NEWLINE +
        ")"
      )
      assert(!fact1.isVacuousYes)

      val fact2 = org.scalatest.expectations.Expectations.expectResult(3) { 3 } && !org.scalatest.expectations.Expectations.expectResult(4) { 4 }
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
      val fact1 = org.scalatest.expectations.Expectations.expectResult(3) { 3 } & org.scalatest.expectations.Expectations.expectResult(3) { 4 }
      assert(fact1.isNo)
      assert(fact1.factMessage  == "3 equaled 3, but 3 did not equal 4")
      assert(fact1.toString ==
        "No(" + NEWLINE +
          "  Yes(expected 3, and got 3) &" + NEWLINE +
          "  No(expected 3, but got 4)" + NEWLINE +
          ")"
      )
      assert(!fact1.isVacuousYes)

      val fact2 = org.scalatest.expectations.Expectations.expectResult(3) { 3 } & !org.scalatest.expectations.Expectations.expectResult(4) { 4 }
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
      val fact = (org.scalatest.expectations.Expectations.expectResult(3) { 3 } && org.scalatest.expectations.Expectations.expectResult(3) { 4 }) || org.scalatest.expectations.Expectations.expectResult(5) { 6 }
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
      val fact = (org.scalatest.expectations.Expectations.expectResult(3) { 3 } & org.scalatest.expectations.Expectations.expectResult(3) { 4 }) | org.scalatest.expectations.Expectations.expectResult(5) { 6 }
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
      val fact = (org.scalatest.expectations.Expectations.expectResult(3) { 3 } && !org.scalatest.expectations.Expectations.expectResult(4) { 4 }) || org.scalatest.expectations.Expectations.expectResult(5) { 6 }
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
      val fact = org.scalatest.expectations.Expectations.expectResult(2) {4} && org.scalatest.expectations.Expectations.expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isNo)
      assert(fact.factMessage == "Expected 2, but got 4")
      assert(!fact.isVacuousYes)
    }
    it("should short-circuit and just return No when used with No && Yes") {
      val fact = org.scalatest.expectations.Expectations.expectResult(2) {4} && org.scalatest.expectations.Expectations.expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isNo)
      assert(fact.factMessage == "Expected 2, but got 4")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_&& when used with Yes && No") {
      val fact = org.scalatest.expectations.Expectations.expectResult(4) {4} && org.scalatest.expectations.Expectations.expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_&&])
      assert(fact.factMessage == "4 equaled 4, but 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_&& when used with Yes && Yes") {
      val fact = org.scalatest.expectations.Expectations.expectResult(4) {4} && org.scalatest.expectations.Expectations.expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_&&])
      assert(fact.factMessage == "4 equaled 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_& when used with No & No") {
      val fact = org.scalatest.expectations.Expectations.expectResult(2) {4} & org.scalatest.expectations.Expectations.expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_&])
      assert(fact.factMessage == "2 did not equal 4, and 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_& when used with No & Yes") {
      val fact = org.scalatest.expectations.Expectations.expectResult(2) {4} & org.scalatest.expectations.Expectations.expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_&])
      assert(fact.factMessage == "2 did not equal 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_& when used with Yes & No") {
      val fact = org.scalatest.expectations.Expectations.expectResult(4) {4} & org.scalatest.expectations.Expectations.expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_&])
      assert(fact.factMessage == "4 equaled 4, but 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_& when used with Yes & Yes") {
      val fact = org.scalatest.expectations.Expectations.expectResult(4) {4} & org.scalatest.expectations.Expectations.expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_&])
      assert(fact.factMessage == "4 equaled 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_|| when used with No || No") {
      val fact = org.scalatest.expectations.Expectations.expectResult(2) {4} || org.scalatest.expectations.Expectations.expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_||])
      assert(fact.factMessage == "2 did not equal 4, and 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_|| when used with No || Yes") {
      val fact = org.scalatest.expectations.Expectations.expectResult(2) {4} || org.scalatest.expectations.Expectations.expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_||])
      assert(fact.factMessage == "2 did not equal 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
    it("should short-circuit and return Yes when used with Yes || No") {
      val fact = org.scalatest.expectations.Expectations.expectResult(4) {4} || org.scalatest.expectations.Expectations.expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isYes)
      assert(fact.factMessage == "Expected 4, and got 4")
      assert(!fact.isVacuousYes)
    }
    it("should short-circuit and return Yes when used with Yes || Yes") {
      val fact = org.scalatest.expectations.Expectations.expectResult(4) {4} || org.scalatest.expectations.Expectations.expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isYes)
      assert(fact.factMessage == "Expected 4, and got 4")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_| when used with No | No") {
      val fact = org.scalatest.expectations.Expectations.expectResult(2) {4} | org.scalatest.expectations.Expectations.expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_|])
      assert(fact.factMessage == "2 did not equal 4, and 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_| when used with No | Yes") {
      val fact = org.scalatest.expectations.Expectations.expectResult(2) {4} | org.scalatest.expectations.Expectations.expectResult(3) {3}
      assert(fact.isInstanceOf[Fact.Binary_|])
      assert(fact.factMessage == "2 did not equal 4, and 3 equaled 3")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_| when used with Yes | No") {
      val fact = org.scalatest.expectations.Expectations.expectResult(4) {4} | org.scalatest.expectations.Expectations.expectResult(3) {5}
      assert(fact.isInstanceOf[Fact.Binary_|])
      assert(fact.factMessage == "4 equaled 4, and 3 did not equal 5")
      assert(!fact.isVacuousYes)
    }
    it("should return Binary_| when used with Yes | Yes") {
      val fact = org.scalatest.expectations.Expectations.expectResult(4) {4} | org.scalatest.expectations.Expectations.expectResult(3) {3}
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
          org.scalatest.expectations.Expectations.expectThrows[MyException] {
            throw new MyException
          } &&
          org.scalatest.expectations.Expectations.expectThrows[MyException] {
            throw new MyExceptionSubClass
          } && {
            // Try with a trait
            trait MyTrait {
              def someRandomMethod(): Unit = {}
            }
            class AnotherException extends RuntimeException with MyTrait
            org.scalatest.expectations.Expectations.expectThrows[MyTrait] {
              throw new AnotherException
            }
          }
        ).isYes
      )
    }
    it("should include the expected exception as the cause so that it will be there if negated") {
        val expectedException = new IllegalArgumentException("I meant to do that!")
        val fact =
          org.scalatest.expectations.Expectations.expectThrows[IllegalArgumentException] {
            throw expectedException
          }
        assert(fact.cause.value eq expectedException)
        assert((!fact).asInstanceOf[Fact.Unary_!].underlying.cause.value eq expectedException)
    }
    describe("when the bit of code throws the wrong exception") {
      it("should include that wrong exception as the Fact's cause") {
        val wrongException = new RuntimeException("oops!")
        val fact =
          org.scalatest.expectations.Expectations.expectThrows[IllegalArgumentException] {
            throw wrongException
          }
        assert(fact.cause.value eq wrongException)
      }
    }
  }

  describe("The expect method") {

    val a = 1

    it("should return Yes when used to check a == 1") {
      val fact = org.scalatest.expectations.Expectations.expect(a == 1)
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isYes)
      assert(fact.factMessage == "1 equaled 1")
      assert(!fact.isVacuousYes)
    }

    it("should return No when used to check a == 2") {
      val fact = org.scalatest.expectations.Expectations.expect(a == 2)
      assert(fact.isInstanceOf[Fact.Leaf])
      assert(fact.isNo)
      assert(fact.factMessage == "1 did not equal 2")
      assert(!fact.isVacuousYes)
    }
  }

  describe("expectDoesNotCompile method") {

    describe("when work with string literal") {

      it("should return Yes with correct fact message when type check failed") {
        val fact = org.scalatest.expectations.Expectations.expectDoesNotCompile("val a: String = 1")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.didNotCompile("val a: String = 1"))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when parse and type check passed") {
        val fact = org.scalatest.expectations.Expectations.expectDoesNotCompile("val a = 1")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        assert(fact.factMessage == Resources.expectedCompileErrorButGotNone("val a = 1"))
        assert(!fact.isVacuousYes)
      }

      it("should return Yes with correct fact messsage when parse failed") {
        val fact = org.scalatest.expectations.Expectations.expectDoesNotCompile("println(\"test)")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.didNotCompile("println(\"test)"))
        assert(!fact.isVacuousYes)
      }

      it("should do nothing when used with 'val i: Int = null'") {
        val fact = org.scalatest.expectations.Expectations.expectDoesNotCompile("val i: Int = null")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
      }
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should return Yes with correct fact message when type check failed") {
        val fact = org.scalatest.expectations.Expectations.expectDoesNotCompile(
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
        val fact = org.scalatest.expectations.Expectations.expectDoesNotCompile(
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
        val fact = org.scalatest.expectations.Expectations.expectDoesNotCompile(
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
    }
  }

  describe("expectCompiles method") {

    describe("when work with string literal") {

      it("should return Yes with correct fact message when type check passed") {
        val fact = org.scalatest.expectations.Expectations.expectCompiles("val a = 1")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.compiledSuccessfully("val a = 1"))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when type check failed") {
        val fact = org.scalatest.expectations.Expectations.expectCompiles("val a: String = 2")
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
        val fact = org.scalatest.expectations.Expectations.expectCompiles("println(\"test)")
        if (ScalaTestVersions.BuiltForScalaVersion == "2.10")
          assert(fact.factMessage == Resources.expectedNoErrorButGotParseError("reflective compilation has failed: \n\nunclosed string literal\n')' expected but eof found.", "println(\"test)"))
        else if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3."))
          assert(fact.factMessage == Resources.expectedNoErrorButGotParseError("expression expected but erroneous token found", "println(\"test)"))
        else
          assert(fact.factMessage == Resources.expectedNoErrorButGotParseError("unclosed string literal", "println(\"test)"))

        assert(!fact.isVacuousYes)
      }
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should return Fact with correct fact message when type check passed") {
        val fact =
          org.scalatest.expectations.Expectations.expectCompiles(
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
          org.scalatest.expectations.Expectations.expectCompiles(
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
          org.scalatest.expectations.Expectations.expectCompiles(
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
    }
  }

  describe("expectTypeError method ") {

    describe("when work with string literal") {

      it("should return Yes with correct fact message when type check failed") {
        val fact = org.scalatest.expectations.Expectations.expectTypeError("val a: String = 1")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isYes)
        assert(fact.factMessage == Resources.gotTypeErrorAsExpected("val a: String = 1"))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when type check passed") {
        val fact = org.scalatest.expectations.Expectations.expectTypeError("val a = 1")
        assert(fact.isInstanceOf[Fact.Leaf])
        assert(fact.isNo)
        assert(fact.factMessage == Resources.expectedTypeErrorButGotNone("val a = 1"))
        assert(!fact.isVacuousYes)
      }

      it("should return No with correct fact message when parse failed") {
        val fact = org.scalatest.expectations.Expectations.expectTypeError("println(\"test)")
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
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should return Yes with correct fact message when type check failed") {
        val fact =
          org.scalatest.expectations.Expectations.expectTypeError(
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
          org.scalatest.expectations.Expectations.expectTypeError(
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
          org.scalatest.expectations.Expectations.expectTypeError(
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
    }
  }
}
